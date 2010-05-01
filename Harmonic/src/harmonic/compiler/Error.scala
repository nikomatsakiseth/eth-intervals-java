package harmonic.compiler

import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position

// For functions which can fail but for which failures should not
// be immediately reported, we often return a CanFail[T],
// which is just a shallow wrapper around Either[Error, T].

abstract class Error {
    def report(state: CompilationState, pos: Position): Unit
}

object Error {
    case class ShadowedClassParam(name: String) extends Error {
        def report(state: CompilationState, pos: Position) {
            state.reporter.report(pos, "shadowed.class.param", name)
        }        
    }
    
    case class ShadowedMethodParam(name: String) extends Error {
        def report(state: CompilationState, pos: Position) {
            state.reporter.report(pos, "shadowed.method.param", name)
        }        
    }
    
    case class ShadowedLocalVar(name: String) extends Error {
        def report(state: CompilationState, pos: Position) {
            state.reporter.report(pos, "shadowed.local.var", name)
        }        
    }
    
    case class CannotResolve(name: String) extends Error {
        def report(state: CompilationState, pos: Position) {
            state.reporter.report(pos, "cannot.resolve", name)
        }
    }
    
    case class DiffStaticClasses(className1: Name.Class, className2: Name.Class) extends Error {
        def report(state: CompilationState, pos: Position) {
            state.reporter.report(pos, "different.static.classes", className1.toString, className2.toString)
        }
    }
    
    case class NoSuchMember(ty: Type.Ref, uName: Name.UnloweredMember) extends Error {
        def report(state: CompilationState, pos: Position) {
            state.reporter.report(pos, "no.such.member", ty.toString, uName.toString)
        }
    }
    
    case class NoSuchMethod(ty: Type.Ref, name: Name.Method) extends Error {
        def report(state: CompilationState, pos: Position) {
            state.reporter.report(pos, "no.such.method", ty.toString, name.toString)
        }
    }
    
    case class QualStatic(memberVar: Name.Member) extends Error {
        def report(state: CompilationState, pos: Position) {
            state.reporter.report(
                pos, "qualified.static",
                memberVar.toString
            )
        }        
    }
    
    case class ExpPath(name: Name.Qual) extends Error {
        def report(state: CompilationState, pos: Position) {
            state.reporter.report(pos, "exp.path.but.found.class", name.toString)
        }
    }
    
    case class ExpClassName(path: String) extends Error {
        def report(state: CompilationState, pos: Position) {
            state.reporter.report(pos, "exp.class", path)
        }
    }
    
    case class ExpStatic(memberVar: Name.Member) extends Error {
        def report(state: CompilationState, pos: Position) {
            state.reporter.report(
                pos, "expected.static",
                memberVar.toString
            )
        }        
    }
    
    case class NotInStaticScope(memberVar: Name.Member) extends Error {
        def report(state: CompilationState, pos: Position) {
            state.reporter.report(
                pos, "not.in.static.context",
                memberVar.toString
            )
        }        
    }
    
    case class AmbiguousMember(options: List[SymTab.MemberEntry]) extends Error {
        def report(state: CompilationState, pos: Position) {
            state.reporter.report(
                pos, "ambiguous.member",
                options.map(_.name).mkString(", ")
            )
        }
    }
    
    case class NotField(name: Name.Member) extends Error {
        def report(state: CompilationState, pos: Position) {
            state.reporter.report(
                pos, "not.a.field", 
                name.toString
            )
        }
    }
    
    case class NotTypeVar(entry: SymTab.MemberEntry) extends Error {
        def report(state: CompilationState, pos: Position) {
            state.reporter.report(
                pos, "not.a.type.var", 
                entry.name.toString
            )
        }
    }
    
    type CanFail[T] = Either[Error, T]
}