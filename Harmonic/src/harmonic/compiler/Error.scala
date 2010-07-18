package harmonic.compiler

import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position
import com.smallcultfollowing.lathos.Context

// For functions which can fail but for which failures should not
// be immediately reported, we often return a CanFail[T],
// which is just a shallow wrapper around Either[Error, T].

abstract class Error {
    def report(global: Global, pos: Position): Unit    
    
    def report(implicit global: Global): Unit = 
        report(global, global.curPos)
}

object Error {
    
    trait ErrorProduct extends Error with Product {
        def args: List[String] = productIterator.toList.map {
            case l: List[_] => l.mkString(", ")
            case o => o.toString
        }
        
        override def report(global: Global, pos: Position): Unit = {
            global.report(pos, getClass.getSimpleName, args: _*)
        }
    }
    
    case class DoesNotPermitWritesFrom(guardPath: Path, interPath: Path) extends ErrorProduct
    case class DoesNotPermitReadsFrom(guardPath: Path, interPath: Path) extends ErrorProduct
    case class NoGhostHere(path: SPath[Phantasmal]) extends ErrorProduct
    case class ExplicitTypeRequiredDueToCycle(memName: String) extends ErrorProduct
    case class NoSuperClassImplements(mthdName: Name.Method) extends ErrorProduct
    case class CanOnlyCreateClasses(ty: Type) extends ErrorProduct
    case class AmbiguousMethodCall(numberOfOptions: Int) extends ErrorProduct
    case class NoApplicableMethods(argTys: List[Type]) extends ErrorProduct
    case class TypeNotFinal(ty: Type) extends ErrorProduct
    case class DoesNotEnsure(fact: inference.Fact) extends ErrorProduct
    case class NoReturnHere() extends ErrorProduct
    case class MustBeSubtype(subTy: Type, supTy: Type) extends ErrorProduct
    case class MustHaveClass(path: SPath[Phantasmal], expClassName: Name.Class) extends ErrorProduct
    case class MustHaveType(path: SPath[Reified], expTy: Type) extends ErrorProduct {
        override def args = List(path.toString, path.ty.toString, expTy.toString)
    }
    case class NotOverride(className: Name.Class, methodName: Name.Method) extends ErrorProduct
    case class ParseError(msg: String) extends ErrorProduct
    case class ReturnWithAndWithoutValue() extends ErrorProduct
    case class CircularInheritance(className: Name.Class, extendsClass: Name.Class) extends ErrorProduct
    case class ExplicitTypeRequiredIfAbstract(methodName: Name.Method) extends ErrorProduct
    case class ExtendsNotEquiv(
        extendedName: Name.Class,
        leftName: Name.Class,
        leftPath: SPath[Reified],
        rightName: Name.Class,
        rightPath: SPath[Reified]
    ) extends ErrorProduct
    case class AmbiguousInheritance(
        className: Name.Class,
        superClasses: List[Name.Class]
    ) extends ErrorProduct
    case class MustResolveAmbiguousInheritance(
        className: Name.Class,
        methodName: Name.Method,
        ambig1: Name.Class,
        ambig2: Name.Class
    ) extends ErrorProduct
    case class MultipleOverridesInSameClass(
        className: Name.Class,
        methodName: Name.Method,
        number: Int
    ) extends ErrorProduct
    case class NotMarkedOverride(
        methodName: Name.Method, 
        classNames: List[Name.Class]
    ) extends ErrorProduct
    case class IoError(err: java.io.IOError) extends ErrorProduct
    case class ShadowedClassParam(name: String) extends ErrorProduct
    case class ShadowedMethodParam(name: String) extends ErrorProduct
    case class ShadowedLocalVar(name: String) extends ErrorProduct
    case class CannotResolve(name: String) extends ErrorProduct
    case class DiffStaticClasses(className1: Name.Class, className2: Name.Class) extends ErrorProduct
    case class NoSuchMember(ty: Type, uName: Name.UnloweredMember) extends ErrorProduct
    case class NoSuchMethod(ty: Type, name: Name.Method) extends ErrorProduct
    case class NoSuchField(name: Name.Member) extends ErrorProduct
    case class NoSuchMethodId(methodId: MethodId) extends ErrorProduct
    case class NotLegalInPathArg(name: Name.Var) extends ErrorProduct
    case class NotLegalInTypeArg(name: Name.Var) extends ErrorProduct
    case class QualStatic(memberVar: Name.Member) extends ErrorProduct
    case class ExpPathNotClass(className: Name.Class) extends ErrorProduct
    case class NoSuchVar(localName: String) extends ErrorProduct
    case class ExpClassName(path: String) extends ErrorProduct
    case class ExpStatic(memberVar: Name.Member) extends ErrorProduct
    case class NotInStaticScope(memberVar: Name.Member) extends ErrorProduct
    case class AmbiguousMember(options: List[SymTab.MemberEntry]) extends ErrorProduct
    case class NotField(name: Name.Member) extends ErrorProduct
    case class NotTypeVar(entry: SymTab.MemberEntry) extends ErrorProduct
    
    type CanFail[T] = Either[Error, T]
}