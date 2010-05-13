package harmonic.compiler

import scala.collection.immutable.Map

import Ast.{Lower => in}
import Util._

class TypedSubst(
    lvmap: Map[VarSymbol.Local, Path.Typed],
    fmap: Map[(VarSymbol.Local, VarSymbol.Field), Path.Typed]
) {
    
    def +(p: Pair[VarSymbol.Local, Path.Typed]): TypedSubst = {
        new TypedSubst(lvmap + p, fmap)
    }
    
    def plusField(p: Pair[(VarSymbol.Local, VarSymbol.Field), Path.Typed]) = {
        new TypedSubst(lvmap, fmap + p)        
    }
    
    def typedPath(path: Path.Typed): Path.Typed = {
        path match {
            // Perform substitutions:
            
            case Path.TypedBase(vsym: VarSymbol.Local) 
            if lvmap.isDefinedAt(vsym) =>
                lvmap(vsym)
        
            case Path.TypedField(Path.TypedBase(vsym: VarSymbol.Local), fsym) 
            if fmap.isDefinedAt((vsym, fsym)) => 
                fmap((vsym, fsym))
                
            // Pass through:
            
            case Path.TypedBase(_) | Path.TypedConstant(_) =>
                path

            case Path.TypedField(base, fsym) => 
                Path.TypedField(typedPath(base), fsym)
                
            case Path.TypedCast(ty, path) =>
                Path.TypedCast(ty, typedPath(path))
            
            case Path.TypedIndex(array, index) =>
                Path.TypedIndex(typedPath(array), typedPath(index))
                
            case Path.TypedTuple(paths) =>
                Path.TypedTuple(paths.map(typedPath))
        }
    }
    
}

object TypedSubst {
    val empty = new TypedSubst(Map(), Map())
}