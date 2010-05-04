package harmonic.compiler

import Ast.{Resolve => in}
import Ast.{Lower => out}
import Util._

object Create {
    def apply(global: Global) = new Create(global)
}

class Create(global: Global) {

    def createMemberIntervals(csym: ClassFromSource) {
        val lower = csym.interval(ClassSymbol.Lower).toList
        val cdecl = csym.resolvedSource
        
        // Create a super interval for the lowering of all members:
        val members = csym.addInterval(ClassSymbol.Members) {
            global.master.subinterval(during = lower) { _ => () }
        }
        
        // Just lower the constructor now.
        val (classParam, classEnv) = Lower(global).classParamAndEnv(csym)
        csym.classParam = classParam
        csym.classEnv = classEnv
        csym.constructor = Lower(global).createSymbolForConstructor(csym)
        
        // Create futures for lowering each member declaration:
        csym.lowerMembers = cdecl.members.map(new LowerMember(global, csym, members, _))

        // Create merge pass that runs after all members have been lowered:
        csym.addInterval(ClassSymbol.Merge) {
            global.master.subinterval(during = lower, after = List(members.end)) { merge =>
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