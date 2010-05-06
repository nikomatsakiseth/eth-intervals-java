package harmonic.compiler

import java.io.File

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Map

import ch.ethz.intervals._

import Ast.{Parse => in}
import Ast.{Resolve => out}
import Util._
import SymTab.extendedMap

/** Header resolution resolves only those parts needed to resolve the
  * rest of the body.  Before we can resolve a class body, we must first 
  * ensure that its headers and those of its superclasses (transitively) have 
  * been resolved. */
case class ResolveHeader(global: Global, compUnit: in.CompUnit) 
extends Resolve(global, compUnit) 
{
    
    def resolveClassHeader(
        csym: ClassFromSource,
        cdecl: in.ClassDecl
    ) {
        // Resolve the names of all superclasses and add an edge
        // so that resolving our header does not complete until
        // their headers have been resolved.  
        csym.superClassNames = cdecl.superClasses.flatMap { relName =>
            val superName = resolveName(relName).name
            val superCsym = global.csym(superName) 
            
            superCsym.optInterval(Pass.Header) match {
                case None => Some(superName)
                case Some(superHeader) => {
                    try {
                        // superHeader.end -> header.end
                        Intervals.addHb(superHeader.end, csym.intervals(Pass.Header).end)
                        
                        // superLower.end -> gather.start
                        superCsym.optInterval(Pass.Lower).foreach { superLower =>
                            Intervals.addHb(superLower.end, csym.intervals(Pass.Gather).start)
                        }
                        
                        // if both succeed, include this supertype:
                        Some(superName)
                    } catch {
                        case _: CycleException => {
                            Error.CircularInheritance(csym.name, superName).report(global, csym.pos)
                            None
                        }
                    }                                    
                }
            }
        }
        
        csym.varMembers = cdecl.members.flatMap {
            case decl: in.IntervalDecl =>
                Some(SymTab.InstanceField(Name.Member(csym.name, decl.name.nm)))
            case decl: in.FieldDecl =>
                Some(SymTab.InstanceField(Name.Member(csym.name, decl.name.nm)))
            case _ =>
                None
        }
    }
    
}