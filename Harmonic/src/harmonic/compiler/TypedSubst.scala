package harmonic.compiler

import scala.collection.immutable.Map

import Ast.{Lower => in}
import Util._

class TypedSubst(
    map: Map[VarSymbol.Any, Path.Typed]
) {
    
    def +(p: Pair[VarSymbol.Local, Path.Typed]) = new TypedSubst(map + p)
    
    def typedPath(path: Path.Typed): Path.Typed = {
        path match {
            case Path.TypedConstant(_) => path
            
            case Path.TypedBase(vsym) =>
                map.get(vsym).getOrElse(path)
        
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
    val empty = new TypedSubst(Map())
}