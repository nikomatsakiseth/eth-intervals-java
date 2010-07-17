package harmonic.compiler

import scala.collection.immutable.Set

import Util._

/** A SPath.Typed is a symbolic version of a Path;
  * in other words, a Path which refers to fields,
  * methods, etc by references to a symbol object
  * rather than by name. 
  *
  * These are generally preferable to a Path, because
  * they are more precise, but they can't be built right
  * away until we have a full set of symbol objects,
  * and they can't be serialized into class files, etc. */
sealed trait SPath {
    def toPath: Path.Ref
    def toPathOwner = toPath
}

object SPath {
    sealed trait Owner {
        def toPathOwner: Path.Owner
    }
    
    case object Static extends Owner {
        def toPathOwner = Path.Static
    }
    
    /** Typed SPath.Typeds represent tangible values that can be
      * used in expressions and elsewhere.  Most SPath.Typeds are
      * typed, except for ghosts. */
    sealed abstract class Typed extends SPath with Owner {
        def ty: Type
        def /(fsym: VarSymbol.Field) = SPath.Field(this, fsym)
    }
    
    object Typed {
        def unapply(symPath: SPath.Typed) = Some(symPath.ty)
    }
    
    case class Local(sym: VarSymbol.Local) extends Typed {
        def toPath = Path.Local(sym.name)
        def ty = sym.ty
        override def toString = toPath.toString
    }
    
    case class Cast(ty: Type, path: SPath.Typed) extends Typed {
        def toPath = Path.Cast(ty, path.toPath)
        override def toString = toPath.toString
    }
    
    case class Constant(obj: Object) extends Typed {
        def toPath = Path.Constant(obj)
        lazy val ty = {
            Type.Class(obj.getClass)
        }
        override def toString = toPath.toString
    }
    
    case class Field(base: SPath.Owner, sym: VarSymbol.Field) extends Typed {
        def toPath = Path.Field(base.toPathOwner, sym.name)
        private[this] lazy val subst = {
            base match {
                case Static => Subst()
                case base: SPath.Typed => Subst(Path.This -> base.toPath)
            }
        }
        lazy val ty = subst.ty(sym.ty)
        lazy val guardPath = subst.path(sym.guardPath)
        override def toString = toPath.toString
    }
    
    case class Ghost(base: SPath.Typed, sym: GhostSymbol) extends SPath {
        def toPath = Path.Field(base.toPathOwner, sym.name) // fields, ghosts same in non-typed form
        override def toString = toPath.toString
    }
    
    case class Call(
        receiver: SPath.Owner, 
        msym: MethodSymbol, 
        args: List[SPath.Typed]
    ) extends Typed {
        assert(sameLength(msym.msig.parameterPatterns.flatMap(_.varNames), args))
    
        lazy val msig = {
            val varNames = msym.msig.parameterPatterns.flatMap(_.varNames)
            var subst = Subst.vt(varNames -> args)
            subst = receiver match {
                case SPath.Static => subst
                case receiver: SPath.Typed => subst + (Path.This -> receiver.toPath)
            }
            subst.methodSignature(msym.msig)
        }
        def ty = msig.returnTy
        def toPath = Path.Call(receiver.toPathOwner, msym.id, args.map(_.toPath))
        override def toString = toPath.toString
    }
    
    case class Index(array: SPath.Typed, index: SPath.Typed) extends Typed {
        def toPath = Path.Index(array.toPath, index.toPath)
    
        lazy val ty = {
            // Special case: constant indices dereferencing a tuple have known
            // type.  Otherwise, we use the type of the array.
            (array.ty, index) match {
                case (Type.Tuple(tys), SPath.Constant(idx: java.lang.Integer)) if idx.intValue < tys.length =>
                    tys(idx.intValue)
            
                case _ => 
                    Type.Member(array.toPath, Name.ArrayElem)
            }
        }
    
        override def toString = toPath.toString
    }
    
    case class Tuple(paths: List[Typed]) extends Typed {
        def toPath = Path.Tuple(paths.map(_.toPath))
        lazy val ty = Type.Tuple(paths.map(_.ty))
        override def toString = toPath.toString
    }

    object Constant {
        def integer(idx: Int) = Constant(java.lang.Integer.valueOf(idx))
    }
}