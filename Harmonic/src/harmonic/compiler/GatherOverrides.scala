package harmonic.compiler

import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.collection.mutable

import Ast.{Lower => in}
import Util._

/** Determines which methods override one another. */
case class GatherOverrides(state: CompilationState) {
    
    /** Populates `csym.methodGroups` as well as the `overrides` 
      * fields of all method symbols defined in `csym` */
    def forSym(csym: Symbol.ClassFromSource) = {
        var env = Env.empty(state) // TODO Enrich environment based on `csym`
        val methodGroups = gatherMethodGroups(csym, env)
        computeOverrides(csym, methodGroups)
        methodGroups.allGroups.foreach(sanityCheckGroup(csym, _))
        csym.methodGroups = methodGroups.allGroups
    }
    
    class MethodGroups(env: Env) {
        private[this] val groups = new mutable.HashMap[Name.Method, List[Symbol.MethodGroup]]()
        
        private[this] def newGroup(msym: Symbol.Method) = {
            val group = new Symbol.MethodGroup(msym.name, msym.msig)
            group.addMsym(msym)
            group
        }
        
        private[this] def isCompatibleWith(
            msig: Symbol.MethodSignature[Pattern.Ref]
        )(
            group: Symbol.MethodGroup
        ) =
            env.overrides(msig, group.msig)
            
        /** Add `msym` to an appropriate group, creating a new one if needed */
        def addMsym(msym: Symbol.Method) = {
            val prevGroups = groups.get(msym.name).getOrElse(Nil)
            prevGroups.find(isCompatibleWith(msym.msig)) match {
                case Some(group) => group.addMsym(msym)
                case None => groups(msym.name) = newGroup(msym) :: prevGroups
            }
        }

        /** All groups defined so far */
        def allGroups = groups.valuesIterator.toList.flatten
        
        /** Returns the group containing `msym` (such a group must exist) */
        def group(msym: Symbol.Method) =
            groups(msym.name).find(isCompatibleWith(msym.msig)).get
    }

    /** Groups all methods defined in `csym` or a supertype 
      * based on common type signature (i.e., overrides) */
    private[this] def gatherMethodGroups(
        csym: Symbol.Class,
        env: Env
    ): MethodGroups = {
        val methodGroups = new MethodGroups(env)
        val mro = MethodResolutionOrder(state).forSym(csym)
        mro.foreach { mroCsym =>
            val msyms = mroCsym.allMethodSymbols(state)
            msyms.foreach(methodGroups.addMsym)
        }        
        methodGroups
    }
    
    /** For each method defined in `csym`, computes the
      * (possibly empty) set of methods that it overrides */
    private[this] def computeOverrides(
        csym: Symbol.Class,
        methodGroups: MethodGroups
    ): Unit = {
        csym.allMethodSymbols(state).foreach { msym =>
            val group = methodGroups.group(msym)
            msym.overrides ++= group.msyms.dropWhile(_.isFromClassNamed(csym.name))
            
            if(msym.overrides.isEmpty && msym.modifiers(state).isOverride) {
                state.reporter.report(
                    msym.pos,
                    "method.does.not.override"
                )
            } else if (!msym.overrides.isEmpty && !msym.modifiers(state).isOverride) {
                val classNames = msym.overrides.map(_.clsName)
                state.reporter.report(
                    msym.pos,
                    "method.must.be.marked.override",
                    msym.name.toString,
                    classNames.mkString(", ")
                )
            }
        }
    }
    
    /** Checks that the contents of a group meet certain conditions:
      * - All */
    private[this] def sanityCheckGroup(
        csym: Symbol.Class,
        group: Symbol.MethodGroup
    ): Unit = {
        
        // Consider the methods in this group defined in `csym` itself:
        group.msyms.takeWhile(_.isFromClassNamed(csym.name)) match {
            
            case List() => {
                // No version of this method defined in `csym`.
                // In that case, all impl. of the method must
                // override a common base method.
                // TODO Implement check for common base method.
                ()
            }

            case List(msym) => {
                ()
            }
            
            case msyms => {
                state.reporter.report(
                    csym.pos,
                    "multiple.overriding.methods.in.same.class",
                    group.methodName.toString,
                    msyms.length.toString
                )                    
            }
            
        }
        
    }
    
}