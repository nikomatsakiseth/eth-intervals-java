package harmonic.compiler

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
        
        // Create futures for lowering each member declaration:
        csym.LowerMembers.v = cdecl.members.map(new LowerMember(global, csym, _))
    }
    
}