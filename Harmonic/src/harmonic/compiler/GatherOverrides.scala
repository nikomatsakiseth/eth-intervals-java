package harmonic.compiler

import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.collection.mutable

import Ast.{Lower => in}
import Util._

/** Determines, for each Method Symbol, what methods are
  * overridden by that symbol. */
case class GatherOverrides(state: CompilationState) {
    
    def forClass(csym: Symbol.ClassFromInterFile) = {
        
        // Construct an environment for `csym`:
        var env = Env.empty(state)
        env = env.plusClassDecl(csym.loweredSource) // XXX Need to add supertypes, etc
        
        // For each method defined in `csym`, go through the methods 
        // with the same name defined in the superclasses and check
        // whether any of them have the same signature:
        val superSyms = MethodResolutionOrder(state).forSym(csym).tail
        val msyms = csym.allMethodSymbols
        for(msym <- msyms; superSym <- superSyms) {
            val superMsyms = superSym.methodsNamed(state)(msym.name)
            msym.overrides ++= superMsyms.filter { superMsym =>
                env.overrides(msym.msig, superMsym.msig)
            }                
        }
        
    }
    
}