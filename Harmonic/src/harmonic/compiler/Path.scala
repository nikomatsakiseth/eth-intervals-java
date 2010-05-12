package harmonic.compiler

import scala.collection.immutable.Set

object Path {
    sealed abstract class Ref
    /** Local variable or static field */
    case class Base(v: Name.Var) extends Ref {
        override def toString = v.toString
    }
    case class Cast(ty: Type.Ref, path: Ref) extends Ref {
        override def toString = "(%s)%s".format(ty, path)
    }
    case class Constant(obj: Object) extends Ref {
        override def toString = obj.toString
    }
    object Constant {
        def integer(idx: Int) = Constant(java.lang.Integer.valueOf(idx))
    }
    /** Instance fields */
    case class Field(base: Path.Ref, f: Name.Member) extends Ref {
        override def toString = base.toString + "." + f.toString
    }
    case class Index(array: Path.Ref, index: Path.Ref) extends Ref {
        override def toString = "%s[%s]".format(array, index)
    }
    
    val This = Path.Base(Name.ThisLocal)    
    val Method = Path.Base(Name.MethodLocal)
    
    sealed abstract class Typed {
        def sym: VarSymbol.Any
        def ty: Type.Ref
        def toPath: Path.Ref
    }
    case class TypedBase(sym: VarSymbol.Any) extends Typed {
        def toPath = Path.Base(sym.name)
        def ty = sym.ty
    }
    case class TypedCast(ty: Type.Ref, path: Typed) extends Typed {
        override def toString = "(%s)%s".format(ty, path)
    }
    case class TypedConstant(obj: Object) extends Typed {
        lazy val ty = {
            Type.Class(obj.getClass)
        }
    }
    object TypedConstant {
        def integer(idx: Int) = TypedConstant(java.lang.Integer.valueOf(idx))
    }
    case class TypedField(base: Path.Typed, sym: VarSymbol.Field) extends Typed {
        def toPath = Path.Field(base.toPath, sym.name)
        lazy val ty = {
            val subst = Subst(Path.This -> base.toPath)
            subst.ty(sym.ty)
        }
    }
    case class TypedIndex(array: Path.Typed, index: Path.Typed) extends Typed {
        def toPath = Path.Index(array.toPath, index.toPath)
        lazy val ty = {
            Type.Var(array.toPath, Name.ArrayElem)
        }
    }
}