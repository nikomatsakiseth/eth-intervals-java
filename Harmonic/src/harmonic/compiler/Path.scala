package harmonic.compiler

import scala.collection.immutable.Set

object Path {
    sealed abstract class Typed {
        def ty: Type.Ref
        def /(fsym: VarSymbol.Field) = Field(this, fsym)
    }
    case class Base(sym: VarSymbol.Any) extends Typed {
        def ty = sym.ty
        override def toString = sym.toString
    }
    case class BaseCall(
        msym: MethodSymbol,
        msig: MethodSignature[Pattern.Anon],
        args: List[Path.Typed]
    ) extends Typed {
        def ty = msig.returnTy
        override def toString = "%s.%s(%s)".format(
            msym.clsName, msym.name.javaName, args.mkString(", ")
        )
    }
    case class Cast(ty: Type.Ref, path: Typed) extends Typed {
        override def toString = "(%s)(%s)".format(
            ty, path
        )
    }
    case class Constant(obj: Object) extends Typed {
        lazy val ty = {
            Type.Class(obj.getClass)
        }
        override def toString = obj.toString
    }
    object Constant {
        def integer(idx: Int) = TypedConstant(java.lang.Integer.valueOf(idx))
    }
    case class Field(base: Path.Typed, fsym: VarSymbol.Field) extends Typed {
        lazy val ty = {
            val thisSym = global.csym(fsym.name.className).thisSym
            val subst = Subst(thisSym -> base)
            subst.ty(sym.ty)
        }
        override def toString = "%s.%s".format(base, sym.name)
    }
    case class Call(
        receiver: Path.Typed, 
        msym: MethodSymbol, 
        msig: MethodSignature[Pattern.Anon], 
        args: List[Path.Typed]
    ) extends Typed {
        def ty = msig.returnTy
        override def toString = "%s.%s(%s)".format(
            receiver, msym.javaName, args.mkString(", ")
        )
    }
    case class Index(array: Path.Typed, index: Path.Typed) extends Typed {
        lazy val ty = {
            Type.Member(array.toPath, Name.ArrayElem)
        }
        override def toString = "%s[%s]".format(
            array, index
        )
    }
    case class Tuple(paths: List[Typed]) extends Typed {
        lazy val ty = Type.Tuple(paths.map(_.ty))
        override def toString = "(%s)".format(
            paths.mkString(", ")
        )
    }
}