package harmonic.compiler

import scala.collection.immutable.Set

import Util._

sealed trait Phantasmal
sealed trait Reified extends Phantasmal
object Phantasmal extends Phantasmal
object Reified extends Reified

/** A SPath[Reified] is a symbolic version of a Path;
  * in other words, a Path which refers to fields,
  * methods, etc by references to a symbol object
  * rather than by name. 
  *
  * These are generally preferable to a Path, because
  * they are more precise, but they can't be built right
  * away until we have a full set of symbol objects,
  * and they can't be serialized into class files, etc. */
sealed trait SPath[+B >: Reified <: Phantasmal]
extends SPath.Owner[B] {
    // For compound SPaths like calls or tuples, the kind 
    // is Reified if all constituents are reified.
    protected[this] def combineKinds(k1: B, spath: SPath[B]) = {
        if(k1 == Phantasmal) k1
        else if(spath.kind == Phantasmal) spath.kind
        else k1 // both are reified
    }
    
    // Distinguishing reified paths from phantasmal ones:
    def kind: B
    def isReified = (kind == Reified)
    def asReified = {
        kind.asInstanceOf[Reified] match {
            case _ => this.asInstanceOf[SPath[Reified]]
        }
    }
    
    // "Unsymbolizing":
    def toPath: Path
    def toPathOwner = toPath
    
    // Extending paths:
    def /(fsym: VarSymbol.Field): SPath[B] = SPath.Field(this, fsym)
    def /(gsym: GhostSymbol): SPath[Phantasmal] = SPath.Ghost(this, gsym)

    // The type of value this path evaluates to:
    def ty: Type
    
    // Substitution:
    override def subst(s: TypedSubst): SPath[B]
}

object SPath {
    case class RefactorTypeException() extends RuntimeException

    sealed trait Owner[+B >: Reified <: Phantasmal] {
        def kind: B
        def toPathOwner: Path.Owner
        def subst(s: TypedSubst): Owner[B]
    }
    
    case object Static extends Owner[Reified] {
        def kind = Reified
        def toPathOwner = Path.Static
        override def subst(s: TypedSubst): Owner[Reified] =
            this
    }
    
    case class Local(
        sym: VarSymbol.Local
    ) extends SPath[Reified] {
        def kind = Reified
        def toPath = Path.Local(sym.name)
        override def ty = sym.ty
        override def toString = toPath.toString
        
        override def subst(s: TypedSubst): SPath[Reified] = {
            s.lvmap.getOrElse(sym, this)
        }
    }
    
    case class Cast[+B >: Reified <: Phantasmal](
        ty: Type, 
        path: SPath[B]
    ) extends SPath[B] {
        def kind = path.kind
        def toPath = Path.Cast(ty, path.toPath)
        override def toString = toPath.toString
        
        override def subst(s: TypedSubst): SPath[B] = {
            Cast(ty, path.subst(s))
        }
    }
    
    case class Constant(
        obj: Object
    ) extends SPath[Reified] {
        def kind = Reified
        def toPath = Path.Constant(obj)
        override def ty = Type.Class(obj.getClass)
        override def toString = toPath.toString
        
        override def subst(s: TypedSubst): SPath[Reified] = 
            this
    }
    
    case class Field[+B >: Reified <: Phantasmal](
        base: SPath.Owner[B], 
        sym: VarSymbol.Field
    ) extends SPath[B] {
        def kind = base.kind
        def toPath = Path.Field(base.toPathOwner, sym.name)
        private[this] lazy val subst = {
            base match {
                case Static => Subst()
                case base: SPath[B] => Subst(Path.This -> base.toPath)
            }
        }
        override def ty = subst.ty(sym.ty)
        lazy val guardPath = subst.path(sym.guardPath)
        override def toString = toPath.toString
        
        override def subst(s: TypedSubst): SPath[B] = {
            base match {
                case SPath.Local(lvsym) if s.fmap.isDefinedAt((lvsym, sym)) =>
                    s.fmap((lvsym, sym))
                case _ =>
                    Field(base.subst(s), sym)
            }
        }
    }
    
    def staticField(fsym: VarSymbol.Field) = {
        assert(fsym.modifiers.isStatic)
        Field[Reified](Static, fsym)
    }
    
    case class Ghost(
        base: SPath[Phantasmal], 
        sym: GhostSymbol
    ) extends SPath[Phantasmal] {
        def kind = Phantasmal
        def toPath = Path.Field(base.toPathOwner, sym.name) // fields, ghosts same in non-typed form
        override def ty = sym.bound.toType
        override def toString = toPath.toString

        override def subst(s: TypedSubst): SPath[Phantasmal] = {
            Ghost(base.subst(s), sym)
        }
    }
    
    case class Call[+B >: Reified <: Phantasmal](
        receiver: SPath.Owner[B], 
        msym: MethodSymbol, 
        args: List[SPath[B]]
    ) extends SPath[B] {
        assert(sameLength(msym.msig.parameterPatterns.flatMap(_.varNames), args))
        def kind = args.foldLeft(receiver.kind)(combineKinds)
    
        lazy val msig = {
            val varNames = msym.msig.parameterPatterns.flatMap(_.varNames)
            var subst = Subst.vt(varNames -> args)
            subst = receiver match {
                case SPath.Static => subst
                case receiver: SPath[_] => subst + (Path.This -> receiver.toPath)
            }
            subst.methodSignature(msym.msig)
        }
        override def ty = msig.returnTy
        def toPath = Path.Call(receiver.toPathOwner, msym.id, args.map(_.toPath))
        override def toString = toPath.toString

        override def subst(s: TypedSubst): SPath[B] = {
            Call(receiver.subst(s), msym, args.map(_.subst(s)))
        }
    }
    
    case class Index[+B >: Reified <: Phantasmal](
        array: SPath[B], 
        index: SPath[B]
    ) extends SPath[B] {
        def kind = combineKinds(array.kind, index)
        def toPath = Path.Index(array.toPath, index.toPath)
    
        override def ty = {
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

        override def subst(s: TypedSubst): SPath[B] = {
            Index(array.subst(s), index.subst(s))
        }
    }
    
    case class Tuple[+B >: Reified <: Phantasmal](
        paths: List[SPath[B]]
    ) extends SPath[B] {
        def kind = paths.foldLeft[B](Reified)(combineKinds)
        def toPath = Path.Tuple(paths.map(_.toPath))
        override def ty = Type.Tuple(paths.map(_.ty))
        override def toString = toPath.toString

        override def subst(s: TypedSubst): SPath[B] = {
            Tuple(paths.map(_.subst(s)))
        }
    }

    object Constant {
        def integer(idx: Int) = Constant(java.lang.Integer.valueOf(idx))
    }
}