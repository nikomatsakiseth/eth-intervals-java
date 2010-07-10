package harmonic.compiler

import ch.ethz.intervals._
import Ast.{Lower => out}
import Util._
import com.smallcultfollowing.lathos.Lathos

object Merge {
    def apply(global: Global) = new Merge(global)
}

class Merge(global: Global) {
    val log = Lathos.context
    
    def mergeMemberIntervals(csym: ClassFromSource) {
        val cdecl = csym.resolvedSource
        val emptyEnv = Env.empty(global)
        
        csym.LoweredMethods.v = csym.lowerMembers.flatMap { 
            case lowerMember: LowerMethodMember => Some((lowerMember.sym, lowerMember.memberDecl))
            case _ => None
        }

        csym.AllFieldSymbols.v = csym.classParam.symbols ++ csym.lowerMembers.flatMap { 
            case lowerMember: LowerFieldMember => Some(lowerMember.sym)
            case lowerMember: LowerIntervalMember => Some(lowerMember.sym)
            case _ => None
        }
        
        csym.AllIntervalSymbols.v = csym.lowerMembers.flatMap { 
            case lowerMember: LowerIntervalMember => Some(lowerMember.sym)
            case _ => None
        }
        
        csym.LoweredSource.v = withPosOf(cdecl, 
            out.ClassDecl(
                name         = cdecl.name,
                annotations  = cdecl.annotations.map(Lower(global).InEnv(emptyEnv).lowerAnnotation),
                extendsDecls = cdecl.extendsDecls.map(Lower(global).InEnv(csym.classEnv).lowerExtendsDecl),
                pattern      = csym.classParam,
                members      = csym.lowerMembers.map(_.memberDecl),
                sym          = csym,
                thisSym      = csym.classEnv.lookupThis
            )
        )

        if(global.config.dumpLoweredTrees) {
            csym.loweredSource.print(PrettyPrinter.debug(log))
        }
    }
    
}