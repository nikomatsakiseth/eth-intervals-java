package harmonic.compiler

import scala.collection.immutable.Map

import Ast.{Lower => in}
import Util._

class TypedSubst(
    lvmap: Map[VarSymbol.Local, SPath.Typed],
    fmap: Map[(VarSymbol.Local, VarSymbol.Field), SPath.Typed]
) {
    
    def +(p: Pair[VarSymbol.Local, SPath.Typed]): TypedSubst = {
        new TypedSubst(lvmap + p, fmap)
    }
    
    def plusField(p: Pair[(VarSymbol.Local, VarSymbol.Field), SPath.Typed]) = {
        new TypedSubst(lvmap, fmap + p)        
    }
    
    def typedOwner(owner: SPath.Owner): SPath.Owner = {
        owner match {
            case SPath.Static => SPath.Static
            case owner: SPath.Typed => typedPath(owner)
        }
    }

    def symPath(spath: SPath): SPath = {
        spath match {
            case SPath.Ghost(path, gsym) => SPath.Ghost(typedPath(path), gsym)
            case spath: SPath.Typed => typedPath(spath)
        }
    }
    
    def typedPath(path: SPath.Typed): SPath.Typed = {
        path match {
            // Perform substitutions:
            
            case SPath.Local(vsym) if lvmap.isDefinedAt(vsym) =>
                lvmap(vsym)
        
            case SPath.Field(SPath.Local(vsym), fsym) if fmap.isDefinedAt((vsym, fsym)) => 
                fmap((vsym, fsym))
                
            // Pass through:
            
            case SPath.Local(_) | SPath.Constant(_) =>
                path

            case SPath.Field(base, fsym) => 
                SPath.Field(typedOwner(base), fsym)
                
            case SPath.Call(receiver, msym, args) =>
                SPath.Call(typedOwner(receiver), msym, args.map(typedPath))

            case SPath.Cast(ty, path) =>
                SPath.Cast(ty, typedPath(path))
            
            case SPath.Index(array, index) =>
                SPath.Index(typedPath(array), typedPath(index))
                
            case SPath.Tuple(paths) =>
                SPath.Tuple(paths.map(typedPath))
        }
    }
    
}

object TypedSubst {
    val empty = new TypedSubst(Map(), Map())
}