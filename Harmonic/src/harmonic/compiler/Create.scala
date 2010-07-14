package harmonic.compiler

import com.smallcultfollowing.lathos.Context
import Ast.{Resolve => in}
import Ast.{Lower => out}
import Util._

object Create {
    def apply(global: Global) = new Create(global)
}

class Create(global: Global) {
    def createMemberIntervals(csym: ClassFromSource) {
        val cdecl = csym.resolvedSource
        
        // Just lower the constructor now.
        val (classParam, classEnv) = Lower(global).classParamAndEnv(csym)
        csym.ClassParam.v = classParam
        csym.ClassEnv.v = classEnv
        csym.Constructor.v = Lower(global).createSymbolForConstructor(csym)
        
        // Determine supertypes:
        // TODO: To accurately determine supertypes, we need a bit more info I guess.
        csym.SuperTypes.v = for(c <- csym.superClassNames) yield Type.Class(c, Nil)
        
        // Determine ghosts:
        csym.AllGhostSymbols.v = cdecl.members.flatMap {
            case decl: in.GhostDecl => Some(new GhostSymbol(decl.pos, decl.name.name, decl.bound.name))
            case _ => None
        }
        
        // Create futures for lowering each member declaration:
        csym.LowerMembers.v = cdecl.members.flatMap {
            case decl: in.IntervalDecl => Some(new LowerIntervalMember(global, csym, decl))
            case decl: in.FieldDecl => Some(new LowerFieldMember(global, csym, decl))
            case decl: in.MethodDecl => Some(new LowerMethodMember(global, csym, decl))
            case decl: in.RelDecl => Some(new LowerRelDecl(global, csym, decl))
            case decl: in.GhostDecl => None
        }
    }
    
}