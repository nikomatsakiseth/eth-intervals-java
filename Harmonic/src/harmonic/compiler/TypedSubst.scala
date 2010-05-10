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
            case Path.TypedBase(vsym) =>
                map.get(vsym).getOrElse(path)
        
            case Path.TypedField(base, fsym) => 
                Path.TypedField(typedPath(base), fsym)
        }
    }
    
    def extendsArg(arg: in.ExtendsArg): in.ExtendsArg = withPosOf(arg, {
        arg match {
            case in.TupleExtendsArg(args) => 
                in.TupleExtendsArg(args.map(extendsArg))
                
            case in.PathExtendsArg(in.TypedPath(path)) => 
                in.PathExtendsArg(in.TypedPath(typedPath(path)))
        }
    })
    
}

object TypedSubst {
    val empty = new TypedSubst(Map())
}