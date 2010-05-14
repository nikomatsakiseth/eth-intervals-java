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
        
        csym.LoweredMethods.v = csym.lowerMembers.flatMap { lmem =>
            lmem.memberDecl match {
                case decl: out.MethodDecl => lmem.toOptMethodSymbol(decl.name).map(s => (s, decl))
                case _ => None
            }
        }

        csym.AllFieldSymbols.v = csym.classParam.symbols ++ csym.lowerMembers.flatMap { lmem =>
            lmem.memberDecl match {
                case decl: out.FieldDecl => lmem.toOptFieldSymbol(decl.name.name)
                case decl: out.IntervalDecl => lmem.toOptFieldSymbol(decl.name.name)
                case _ => None
            }
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
            csym.loweredSource.println(PrettyPrinter.stdout)
        }
    }
    
}