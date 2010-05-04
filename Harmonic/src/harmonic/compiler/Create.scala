package harmonic.compiler

import Ast.{Resolve => in}
import Ast.{Lower => out}

object Create {
    def apply(global: Global) = new Create(global)
}

class Create(global: Global) {

    def createMemberIntervals(csym: ClassFromSource) {
        val lower = csym.interval(ClassSymbol.Lower).toList
        val cdecl = csym.resolvedSource
        
        // Create a super interval for the lowering of all members:
        val members = csym.addInterval(ClassSymbol.Members) {
            master.subinterval(during = lower) { _ => () }
        }
        
        // Just lower the constructor now.
        csym.classParamAndEnv = Lower(global).classParamAndEnv
        csym.ctorSymbol = Lower(global).createSymbolForConstructor()
        
        // Create futures for lowering each member declaration:
        csym.lowerMembers = cdecl.members.map(new LowerMember(global, members, _))

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