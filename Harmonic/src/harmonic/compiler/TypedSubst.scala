package harmonic.compiler

import scala.collection.immutable.Map

import Ast.{Lower => in}
import Util._

class TypedSubst(
    lvmap: Map[VarSymbol.Local, Path.Typed],
    fmap: Map[(VarSymbol.Local, VarSymbol.Field), Path.Typed]
) {
    
    private[this] lazy val untypedSubst = {
        val subst = lvmap.foldLeft(Subst.empty) { case (s, (l, t)) =>
            s + (l.toPath -> t.toPath)
        }
        fmap.foldLeft(subst) { case (s, ((l, f), t)) =>
            s + (Path.Field(l.toPath, f.name) -> t.toPath)
        }
    }
    
    def +(p: Pair[VarSymbol.Local, Path.Typed]): TypedSubst = {
        new TypedSubst(lvmap + p, fmap)
    }
    
    def plusField(p: Pair[(VarSymbol.Local, VarSymbol.Field), Path.Typed]) = {
        new TypedSubst(lvmap, fmap + p)        
    }
    
    def methodSignature(msig: MethodSignature[Pattern.Anon]) = {
        untypedSubst.methodSignature(msig)
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
                
            case Path.TypedBaseCall(msym, msig, args) =>
                Path.TypedBaseCall(msym, methodSignature(msig), args.map(typedPath))
                
            case Path.TypedCall(receiver, msym, msig, args) =>
                Path.TypedCall(typedPath(receiver), msym, methodSignature(msig), args.map(typedPath))
                
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