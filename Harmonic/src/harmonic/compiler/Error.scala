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
    case class NoSuchMember(ty: Type.Ref, uName: Name.UnloweredMemberVar) extends Error {
        def report(state: CompilationState, pos: Position) {
            state.reporter.report(pos, "no.such.member", ty.toString, uName.toString)
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
    
    case class NotField(entry: SymTab.MemberEntry) extends Error {
        def report(state: CompilationState, pos: Position) {
            state.reporter.report(
                pos, "not.a.field", 
                entry.name.toString
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