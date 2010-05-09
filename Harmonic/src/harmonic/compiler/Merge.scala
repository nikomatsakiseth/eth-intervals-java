package harmonic.compiler

import ch.ethz.intervals._
import Ast.{Lower => out}
import Util._

object Merge {
    def apply(global: Global) = new Merge(global)
}

class Merge(global: Global) {
    
    def mergeMemberIntervals(csym: ClassFromSource) {
        val cdecl = csym.resolvedSource
        val emptyEnv = Env.empty(global)
        
        csym.loweredMethods = csym.lowerMembers.flatMap { lmem =>
            lmem.memberDecl match {
                case decl: out.MethodDecl => lmem.toOptMethodSymbol(decl.name).map(s => (s, decl))
                case _ => None
            }
        }

        csym.allFieldSymbols = csym.classParam.symbols ++ csym.lowerMembers.flatMap { lmem =>
            lmem.memberDecl match {
                case decl: out.FieldDecl => lmem.toOptFieldSymbol(decl.name.name)
                case decl: out.IntervalDecl => lmem.toOptFieldSymbol(decl.name.name)
                case _ => None
            }
        }
        
        csym.loweredSource = withPosOf(cdecl, 
            out.ClassDecl(
                name         = cdecl.name,
                annotations  = cdecl.annotations.map(Lower(global).InEnv(emptyEnv).lowerAnnotation),
                superClasses = cdecl.superClasses,
                pattern      = csym.classParam,
                members      = csym.lowerMembers.map(_.memberDecl),
                sym          = csym,
                thisSym      = csym.classEnv.lookupThis
            )
        )
        

        if(global.config.dumpLoweredTrees) {
            csym.loweredSource.println(PrettyPrinter.stdout)
        }
    }
    
}