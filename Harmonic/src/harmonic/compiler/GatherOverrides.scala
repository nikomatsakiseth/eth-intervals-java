package harmonic.compiler

import scala.collection.immutable.Map
import scala.collection.immutable.Set
import scala.collection.mutable

import com.smallcultfollowing.lathos.model.Context
import Ast.{Lower => in}
import Util._

/** Determines which methods override one another. */
case class GatherOverrides(global: Global, log: Context) {
    
    /** Populates `csym.methodGroups` as well as the `overrides` 
      * fields of all method symbols defined in `csym` */
    def forSym(csym: ClassSymbol): Unit = {
        
        // The full dependencies for GatherOverrides are:
        //
        // 1. Must not run until lowering for all supertypes is
        //    complete.  This is assured because it comes after
        //    lower, and super.lower.end->lower.end.
        //
        // 2. GatherOverrides must execute on all supertypes
        //    so that the supertype's MethodSymbols have
        //    their overrides fields initialized.
        //    This is assured by ResolveHeader which adds
        //    edges super.gather.end->gather.start.
        
        var env = csym.checkEnv
        val methodGroups = gatherMethodGroups(csym, env)
        computeOverrides(csym, methodGroups)
        val allGroups = methodGroups.allGroups
        allGroups.foreach(sanityCheckGroup(csym, _))
        csym.setMethodGroups(allGroups)
    }
    
    class MethodGroups(env: Env) {
        private[this] val groups = new mutable.HashMap[Name.Method, List[MethodGroup]]()
        
        private[this] def newGroup(msym: MethodSymbol) = {
            val group = new MethodGroup(msym.name, msym.msig)
            group.addMsym(msym)
            group
        }
        
        private[this] def isCompatibleWith(
            msig: MethodSignature[Pattern.Ref]
        )(
            group: MethodGroup
        ) = {
            env.overrides(msig, group.msig)            
        }
            
        /** Add `msym` to an appropriate group, creating a new one if needed */
        def addMsym(msym: MethodSymbol) = {
            val prevGroups = groups.get(msym.name).getOrElse(Nil)
            prevGroups.find(isCompatibleWith(msym.msig)) match {
                case Some(group) => group.addMsym(msym)
                case None => groups(msym.name) = newGroup(msym) :: prevGroups
            }
        }

        /** All groups defined so far */
        def allGroups = groups.valuesIterator.toList.flatten
        
        /** Returns the group containing `msym` (such a group must exist) */
        def group(msym: MethodSymbol) =
            groups(msym.name).find(isCompatibleWith(msym.msig)).get
    }

    /** Groups all methods defined in `csym` or a supertype 
      * based on common type signature (i.e., overrides) */
    private[this] def gatherMethodGroups(
        csym: ClassSymbol,
        env: Env
    ): MethodGroups = {
        val methodGroups = new MethodGroups(env)
        val mro = MethodResolutionOrder(global).forSym(csym)
        mro.foreach { mroCsym =>
            val msyms = mroCsym.allMethodSymbols
            msyms.foreach(methodGroups.addMsym)
        }        
        methodGroups
    }
    
    /** For each method defined in `csym`, computes the
      * (possibly empty) set of methods that it overrides */
    private[this] def computeOverrides(
        csym: ClassSymbol,
        methodGroups: MethodGroups
    ): Unit = {
        csym.allMethodSymbols.foreach { msym =>
            val group = methodGroups.group(msym)
            msym.Overrides.v = group.msyms.dropWhile(_.isFromClassNamed(csym.name))
            
            msym.kind match {
                case _: MethodKind.Harmonic => {
                    if(msym.overrides.isEmpty && msym.modifiers.isOverride) {
                        Error.NotOverride(csym.name, msym.name).report(global, log, msym.pos)
                    } else if (!msym.overrides.isEmpty && !msym.modifiers.isOverride) {
                        val classNames = msym.overrides.map(_.clsName).toList
                        Error.NotMarkedOverride(msym.name, classNames).report(global, log, msym.pos)
                    }                    
                }
                
                case _: MethodKind.Java 
                |   MethodKind.JavaDummyCtor 
                |   MethodKind.ErrorMethod => {
                }
            }
        }
    }
    
    /** Checks that the contents of a group meet certain conditions:
      * - All */
    private[this] def sanityCheckGroup(
        csym: ClassSymbol,
        group: MethodGroup
    ): Unit = {
        
        // Consider the methods in this group defined in `csym` itself:
        group.msyms.takeWhile(_.isFromClassNamed(csym.name)) match {
            
            case List() => {
                // No version of this method defined in `csym`.
                // In that case, all impl. of the method must
                // override a common base method.
                val tops = group.msyms.foldLeft(group.msyms) {
                    case (l, msym) => l.filterNot(msym.overrides.contains)
                }
                val reported = new mutable.HashSet[(MethodSymbol, MethodSymbol)]()
                (tops cross tops).foreach { case (msym1, msym2) =>
                    if(msym1 != msym2 && !msym1.overrides.intersects(msym2.overrides)) {
                        if(reported.add((msym1, msym2)) && reported.add((msym2, msym1))) {
                            Error.MustResolveAmbiguousInheritance(
                                csym.name, group.methodName, 
                                msym1.clsName, msym2.clsName
                            ).report(global, log, csym.pos)
                        }
                    }
                }
            }

            case List(msym) => {
                ()
            }
            
            case msyms => {
                Error.MultipleOverridesInSameClass(
                    csym.name, group.methodName, msyms.length
                ).report(global, log, csym.pos)
            }
            
        }
        
    }
    
}