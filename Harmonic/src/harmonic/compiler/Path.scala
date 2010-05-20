package harmonic.compiler

import scala.collection.immutable.Set

object Path {
    sealed abstract class Ref
    /** Local variable or static field */
    case class Base(v: Name.Var) extends Ref {
        override def toString = v.toString
    }
    /** Static call */
    case class BaseCall(className: Name.Class, methodName: Name.Method, args: List[Path.Ref]) extends Ref {
        override def toString = "%s.%s(%s)".format(className, methodName.javaName, args.mkString(", "))
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
    /** Virtual call, parameters flatened */
    case class Call(receiver: Path.Ref, methodName: Name.Method, args: List[Path.Ref]) extends Ref {
        override def toString = "%s.%s(%s)".format(base, methodName.javaName, args.mkString(", "))
    }
    case class Index(array: Path.Ref, index: Path.Ref) extends Ref {
        override def toString = "%s[%s]".format(array, index)
    }
    case class Tuple(paths: List[Ref]) extends Ref {
        override def toString = "(%s)".format(paths.mkString(","))
    }
    
    val This = Path.Base(Name.ThisLocal)    
    val Method = Path.Base(Name.MethodLocal)
    
    sealed abstract class Typed {
        def ty: Type.Ref
        def toPath: Path.Ref
        def /(fsym: VarSymbol.Field) = TypedField(this, fsym)
    }
    case class TypedBase(sym: VarSymbol.Any) extends Typed {
        def toPath = Path.Base(sym.name)
        def ty = sym.ty
        override def toString = toPath.toString
    }
    case class TypedBaseCall(
        className: Name.Class, 
        msym: MethodSymbol,
        msig: MethodSignature,
        args: List[Path.Typed]
    ) extends Ref {
        override def toString = "%s.%s(%s)".format(className, methodName.javaName, args.mkString(", "))
    }
    case class TypedCast(ty: Type.Ref, path: Typed) extends Typed {
        def toPath = Path.Cast(ty, path.toPath)
        override def toString = toPath.toString
    }
    case class TypedConstant(obj: Object) extends Typed {
        def toPath = Constant(obj)
        lazy val ty = {
            Type.Class(obj.getClass)
        }
        override def toString = toPath.toString
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
        override def toString = toPath.toString
    }
    case class TypedCall(
        receiver: Path.Typed, 
        msym: MethodSymbol, 
        msig: MethodSignature, 
        args: List[Path.Typed]
    ) extends Ref {
        def toPath = Path.Call(receiver.toPath, msym.name, args.map(_.toPath))
        override def toString = toPath.toString
    }
    case class TypedIndex(array: Path.Typed, index: Path.Typed) extends Typed {
        def toPath = Index(array.toPath, index.toPath)
        lazy val ty = {
            Type.Member(array.toPath, Name.ArrayElem)
        }
        override def toString = toPath.toString
    }
    case class TypedTuple(paths: List[Typed]) extends Typed {
        def toPath = Tuple(paths.map(_.toPath))
        lazy val ty = Type.Tuple(paths.map(_.ty))
        override def toString = toPath.toString
    }
}