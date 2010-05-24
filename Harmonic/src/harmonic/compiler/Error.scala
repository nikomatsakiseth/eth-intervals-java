package harmonic.compiler

import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position

// For functions which can fail but for which failures should not
// be immediately reported, we often return a CanFail[T],
// which is just a shallow wrapper around Either[Error, T].

abstract class Error {
    def report(global: Global, pos: Position): Unit
}

object Error {
    case class CircularMethodType(className: Name, methodName: Name.Method) extends Error {
        def report(global: Global, pos: Position) {
            global.reporter.report(pos, 
                "circular.method.type", className.toString, methodName.toString
            )
        }                
    }
    
    case class CircularMemberType(memberName: Name.Member) extends Error {
        def report(global: Global, pos: Position) {
            global.reporter.report(pos, 
                "circular.member.type", memberName.toString
            )
        }                
    }
    
    case class NotOverride(className: Name.Class, methodName: Name.Method) extends Error {
        def report(global: Global, pos: Position) {
            global.reporter.report(pos, 
                "method.does.not.override", className.toString, methodName.toString
            )
        }                
    }
    
    case class ParseError(
        msg: String
    ) {
        def report(global: Global, pos: Position) {
        }                
    }
    
    case class ReturnWithAndWithoutValue() {
        def report(global: Global, pos: Position) {
            global.reporter.report(pos,
                "return.with.and.without.value"
            )
        }
    }
    
    case class CircularInheritance(
        className: Name.Class, 
        extendsClass: Name.Class
    ) {
        def report(global: Global, pos: Position) {
            global.reporter.report(pos,
                "circular.inheritance",
                className.toString, 
                extendsClass.toString
            )
        }                
    }
    
    case class ExtendsNotEquiv(
        extendedName: Name.Class,
        leftName: Name.Class,
        leftPath: Path.Typed,
        rightName: Name.Class,
        rightPath: Path.Typed
    ) extends Error {
        def report(global: Global, pos: Position) {
            global.reporter.report(pos, 
                "extends.not.equiv",
                extendedName.toString,
                leftName.toString, leftPath.toString,
                rightName.toString, rightPath.toString
            )
        }
    }

    case class AmbiguousInheritance(
        className: Name.Class,
        superClasses: List[Name.Class]
    ) extends Error {
        def report(global: Global, pos: Position) {
            global.reporter.report(pos, 
                "ambiguous.inheritance",
                className.toString, 
                superClasses.mkString(", ")
            )
        }                
    }
    
    case class MustResolveAmbiguousInheritance(
        className: Name.Class,
        methodName: Name.Method,
        ambig1: Name.Class,
        ambig2: Name.Class
    ) extends Error {
        def report(global: Global, pos: Position) {
            global.reporter.report(pos, 
                "must.resolve.ambiguity",
                className.toString,
                methodName.toString,
                ambig1.toString,
                ambig2.toString
            )
        }                
    }
    
    case class MultipleOverridesInSameClass(
        className: Name.Class,
        methodName: Name.Method,
        number: Int
    ) extends Error {
        def report(global: Global, pos: Position) {
            global.reporter.report(pos, 
                "multiple.overriding.methods.in.same.class",
                className.toString,
                methodName.toString,
                number.toString
            )
        }                
    }
    
    case class NotMarkedOverride(
        methodName: Name.Method, 
        classNames: List[Name.Class]
    ) extends Error {
        def report(global: Global, pos: Position) {
            global.reporter.report(pos, 
                "method.must.be.marked.override",
                methodName.toString,
                classNames.mkString(", ")
            )
        }                
        
    }
    
    case class IOError(err: java.io.IOError) extends Error {
        def report(global: Global, pos: Position) {
            global.reporter.report(pos, 
                "io.error",
                err.toString
            )
        }                
    }
    
    case class ShadowedClassParam(name: String) extends Error {
        def report(global: Global, pos: Position) {
            global.reporter.report(pos, 
                "shadowed.class.param", 
                name
            )
        }        
    }
    
    case class ShadowedMethodParam(name: String) extends Error {
        def report(global: Global, pos: Position) {
            global.reporter.report(pos, 
                "shadowed.method.param", 
                name
            )
        }        
    }
    
    case class ShadowedLocalVar(name: String) extends Error {
        def report(global: Global, pos: Position) {
            global.reporter.report(pos, 
                "shadowed.local.var", 
                name
            )
        }        
    }
    
    case class CannotResolve(name: String) extends Error {
        def report(global: Global, pos: Position) {
            global.reporter.report(pos, 
                "cannot.resolve", 
                name
            )
        }
    }
    
    case class DiffStaticClasses(className1: Name.Class, className2: Name.Class) extends Error {
        def report(global: Global, pos: Position) {
            global.reporter.report(pos, 
                "different.static.classes", 
                className1.toString, className2.toString
            )
        }
    }
    
    case class NoSuchMember(ty: Type.Ref, uName: Name.UnloweredMember) extends Error {
        def report(global: Global, pos: Position) {
            global.reporter.report(pos, 
                "no.such.member", 
                ty.toString, uName.toString
            )
        }
    }
    
    case class NoSuchMethod(ty: Type.Ref, name: Name.Method) extends Error {
        def report(global: Global, pos: Position) {
            global.reporter.report(pos, 
                "no.such.method", 
                ty.toString, name.toString
            )
        }
    }
    
    case class QualStatic(memberVar: Name.Member) extends Error {
        def report(global: Global, pos: Position) {
            global.reporter.report(pos,
                "qualified.static",
                memberVar.toString
            )
        }        
    }
    
    case class ExpPathNotClass(className: Name.Class) extends Error {
        def report(global: Global, pos: Position) {
            global.reporter.report(pos, "exp.path.but.found.class", className.toString)
        }
    }
    
    case class NoSuchVar(localName: String) extends Error {
        def report(global: Global, pos: Position) {
            global.reporter.report(pos, "no.such.var", localName)
        }
    }
    
    case class ExpClassName(path: String) extends Error {
        def report(global: Global, pos: Position) {
            global.reporter.report(pos, "exp.class", path)
        }
    }
    
    case class ExpStatic(memberVar: Name.Member) extends Error {
        def report(global: Global, pos: Position) {
            global.reporter.report(pos, "expected.static", memberVar.toString)
        }        
    }
    
    case class NotInStaticScope(memberVar: Name.Member) extends Error {
        def report(global: Global, pos: Position) {
            global.reporter.report(pos, 
                "not.in.static.context", 
                memberVar.toString)
        }        
    }
    
    case class AmbiguousMember(options: List[SymTab.MemberEntry]) extends Error {
        def report(global: Global, pos: Position) {
            global.reporter.report(pos, 
                "ambiguous.member",
                options.map(_.name).mkString(", ")
            )
        }
    }
    
    case class NotField(name: Name.Member) extends Error {
        def report(global: Global, pos: Position) {
            global.reporter.report(pos, 
                "not.a.field", 
                name.toString
            )
        }
    }
    
    case class NotTypeVar(entry: SymTab.MemberEntry) extends Error {
        def report(global: Global, pos: Position) {
            global.reporter.report(pos, 
                "not.a.type.var", 
                entry.name.toString
            )
        }
    }
    
    type CanFail[T] = Either[Error, T]
}