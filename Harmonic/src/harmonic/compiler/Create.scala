package harmonic.compiler

import Ast.{Resolve => in}
import Ast.{Lower => out}

object Create {
    def apply(state: State) = new Create(state)
}

class Create(state: State) {

    def createMemberIntervals() {
        val csym = state.curCsym
        val lower = csym.interval(ClassSymbol.Lower).toList
        val cdecl = csym.resolvedSource
        
        // Create a super interval for the lowering of all members:
        val members = csym.addInterval(ClassSymbol.Members) {
            master.subinterval(during = lower) { _ => () }
        }
        
        // Just lower the constructor now.
        csym.classParamAndEnv = Lower(state).classParamAndEnv
        csym.ctorSymbol = Lower(state).createSymbolForConstructor()
        
        // Create futures for lowering each member declaration:
        csym.lowerMembers = cdecl.members.map(new LowerMember(state, members, _))

        // Create merge pass that runs after all members have been lowered:
        csym.addInterval(ClassSymbol.Merge) {
            master.subinterval(during = lower, after = List(members)) { merge =>
                csym.loweredSource = withPosOf(cdecl, 
                    out.ClassDecl(
                        name         = cdecl.name,
                        annotations  = cdecl.annotations.map(InScope(emptyEnv).lowerAnnotation),
                        superClasses = cdecl.superClasses,
                        pattern      = outParam,
                        members      = memberFutures.map(_.now),
                        sym          = csym
                    )
                )
            }
        }
    }
    
}