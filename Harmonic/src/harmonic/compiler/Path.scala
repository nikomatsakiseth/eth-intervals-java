package harmonic.compiler

import scala.collection.immutable.Set

import Util._

object Path {
    
    sealed abstract trait UntypedOwner
    sealed abstract trait TypedOwner {
        def toOwner: UntypedOwner
    }
    case object Static extends TypedOwner with UntypedOwner {
        def toOwner = this
    }
    
    // ___ Untyped Paths ____________________________________________________
    
    sealed abstract class Ref extends UntypedOwner {
        def is(r: Ref) = (this == r)
        def /(fname: Name.Member) = Field(this, fname)
    }
    case class Local(v: Name.LocalVar) extends Ref {
        override def toString = v.toString
    }
    case class Cast(ty: Type.Ref, path: Ref) extends Ref {
        override def toString = "(%s)%s".format(ty, path)
    }
    case class Constant(obj: Object) extends Ref {
        override def toString = obj match {
            case obj: String => '"' + obj.toString + '"'
            case _ => obj.toString
        }
    }
    case class Field(base: UntypedOwner, f: Name.Member) extends Ref {
        override def toString = base.toString + "." + f.toString
    }
    case class Call(receiver: UntypedOwner, methodId: MethodId, args: List[Path.Ref]) extends Ref {
        override def toString = "%s.%s(%s)".format(receiver, methodId, args.mkString(", "))
    }
    case class Index(array: Path.Ref, index: Path.Ref) extends Ref {
        override def toString = "%s[%s]".format(array, index)
    }
    case class Tuple(paths: List[Ref]) extends Ref {
        override def toString = "(%s)".format(paths.mkString(","))
    }
    
    object Constant {
        def integer(idx: Int) = Constant(java.lang.Integer.valueOf(idx))
    }
    
    val This = Name.ThisLocal.toPath
    val Method = Name.MethodLocal.toPath
    val Final = Name.FinalLocal.toPath
    val ThisInit = This / Name.Init
    
    // ___ Typed Paths ______________________________________________________
    
    sealed abstract class Typed extends TypedOwner {
        def ty: Type.Ref
        def toPath: Path.Ref
        def toOwner = toPath
        def /(fsym: VarSymbol.Field) = TypedField(this, fsym)
    }
    case class TypedLocal(sym: VarSymbol.Local) extends Typed {
        def toPath = Path.Local(sym.name)
        def ty = sym.ty
        override def toString = toPath.toString
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
    case class TypedField(base: Path.TypedOwner, sym: VarSymbol.Field) extends Typed {
        def toPath = Path.Field(base.toOwner, sym.name)
        lazy val ty = {
            base match {
                case Static => sym.ty
                case base: Path.Typed => Subst(Path.This -> base.toPath).ty(sym.ty)
            }
        }
        override def toString = toPath.toString
    }
    case class TypedCall(
        receiver: Path.TypedOwner, 
        msym: MethodSymbol, 
        args: List[Path.Typed]
    ) extends Typed {
        assert(sameLength(msym.msig.parameterPatterns.flatMap(_.varNames), args))
        
        lazy val msig = {
            val varNames = msym.msig.parameterPatterns.flatMap(_.varNames)
            var subst = Subst.vt(varNames -> args)
            subst = receiver match {
                case Path.Static => subst
                case receiver: Path.Typed => subst + (Path.This -> receiver.toPath)
            }
            subst.methodSignature(msym.msig)
        }
        def ty = msig.returnTy
        def toPath = Path.Call(receiver.toOwner, msym.id, args.map(_.toPath))
        override def toString = toPath.toString
    }
    case class TypedIndex(array: Path.Typed, index: Path.Typed) extends Typed {
        def toPath = Index(array.toPath, index.toPath)
        
        lazy val ty = {
            // Special case: constant indices dereferencing a tuple have known
            // type.  Otherwise, we use the type of the array.
            (array.ty, index) match {
                case (Type.Tuple(tys), Path.TypedConstant(idx: java.lang.Integer)) if idx.intValue < tys.length =>
                    tys(idx.intValue)
                
                case _ => 
                    Type.Member(array.toPath, Name.ArrayElem)
            }
        }
        
        override def toString = toPath.toString
    }
    case class TypedTuple(paths: List[Typed]) extends Typed {
        def toPath = Tuple(paths.map(_.toPath))
        lazy val ty = Type.Tuple(paths.map(_.ty))
        override def toString = toPath.toString
    }
    
    object TypedConstant {
        def integer(idx: Int) = TypedConstant(java.lang.Integer.valueOf(idx))
    }
}