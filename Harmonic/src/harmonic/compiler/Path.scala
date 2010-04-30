package harmonic.compiler

import scala.collection.immutable.Set

object Path {
    sealed abstract class Ref
    case class Base(v: Name.Var) extends Ref {
        override def toString = v.toString
    }
    case class Field(base: Path.Ref, f: Name.MemberVar) extends Ref {
        override def toString = base.toString + f.toString
    }
    
    val This = Path.Base(Name.ThisLocal)    
    val Method = Path.Base(Name.MethodLocal)
    
    sealed abstract class Typed {
        def sym: Symbol.Var
        def ty: Type.Ref
        def toPath: Path.Ref
    }
    case class TypedBase(sym: Symbol.Var) extends Typed {
        def toPath = Path.Base(sym.name)
        def ty = sym.ty
    }
    case class TypedField(base: Path.Typed, sym: Symbol.Field) extends Typed {
        def toPath = Path.Field(base.toPath, sym.name)
        lazy val ty = {
            val subst = Subst(Path.This -> base.toPath)
            subst.ty(sym.ty)
        }
    }
}