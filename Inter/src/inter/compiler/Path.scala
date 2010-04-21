package inter.compiler

import scala.collection.immutable.Set

object Path {
    sealed abstract class Ref
    case class Base(v: Name.Var) extends Ref {
        override def toString = v.toString
    }
    case class Field(base: Path.Ref, f: Name.Var) extends Ref {
        override def toString = base.toString + f.toString
    }
    
    val This = Path.Base(Name.ThisVar)    
    val Method = Path.Base(Name.MethodVar)
    
    sealed abstract class Typed {
        def sym: Symbol.Var
        def ty: Type.Ref
        def toPath: Path.Ref
    }
    case class TypedBase(v: Name.Var, sym: Symbol.Var, ty: Type.Ref) extends Typed {
        def toPath = Path.Base(v)
    }
    case class TypedField(base: Path.Typed, sym: Symbol.Var, ty: Type.Ref) extends Typed {
        def toPath = Path.Field(base.toPath, sym.name)
    }
}