package harmonic.compiler

import java.io.File

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Map

import ch.ethz.intervals._
import com.smallcultfollowing.lathos.model.Context

import Ast.{Parse => in}
import Ast.{Resolve => out}
import Util._
import SymTab.extendedMap

object ResolveHeader {
    
    /** Given a list of superclass names, adds edges from the superclass
      * intervals to our own where needed.  The returned list of superclass
      * names omits those that cause a cycle. */
    def cookRawSuperClasses(csym: ClassSymbol, rawSuperClassNames: List[Name.Class]) = {
        import csym.global
        
        rawSuperClassNames.flatMap { superName =>
            val superCsym = global.csym(superName) 
            
            try {
                Intervals.addHb(superCsym.header.getEnd, csym.header.getEnd)
                Intervals.addHb(superCsym.lower.getEnd, csym.lower.getEnd)
                Intervals.addHb(superCsym.envirate.getEnd, csym.envirate.getStart)
                Intervals.addHb(superCsym.check.getEnd, csym.check.getStart)
                Intervals.addHb(superCsym.gather.getEnd, csym.gather.getStart)
                Some(superName)
            } catch {
                case _: IntervalException.Cycle => {
                    Error.CircularInheritance(csym.name, superName).report(global, csym.pos)
                    None
                }
            }                                    
        }
    }
    
}

/** Header resolution resolves only those parts needed to resolve the
  * rest of the body.  Before we can resolve a class body, we must first 
  * ensure that its headers and those of its superclasses (transitively) have 
  * been resolved. */
case class ResolveHeader(global: Global, compUnit: in.CompUnit, log: Context) 
extends Resolve(global, compUnit) 
{
    
    def resolveClassHeader(
        csym: ClassFromSource,
        cdecl: in.ClassDecl
    ): Unit = {
        // Resolve the names of all superclasses and add an edge
        // so that resolving our header does not complete until
        // their headers have been resolved.  
        val rawSuperClassNames = cdecl.extendsDecls.map { case in.ExtendsDecl(relName, _, ()) =>
            resolveName(relName).name
        }
        csym.SuperClassNames.v = ResolveHeader.cookRawSuperClasses(csym, rawSuperClassNames)
        
        def addVarMembersFromMember(varMembers: List[SymTab.Entry], decl: in.MemberDecl): List[SymTab.Entry] = {
            decl match {
                case decl: in.IntervalDecl =>
                    SymTab.InstanceField(Name.Member(csym.name, decl.name.nm)) :: varMembers
                case decl: in.FieldDecl =>
                    SymTab.InstanceField(Name.Member(csym.name, decl.name.nm)) :: varMembers
                case _ =>
                    varMembers                
            }
        }
        
        def addVarMembersFromParam(varMembers: List[SymTab.Entry], param: in.Param[Unit]): List[SymTab.Entry] = {
            param match {
                case in.TupleParam(params) => 
                    params.foldLeft(varMembers)(addVarMembersFromParam)
                case in.VarParam(_, _, Ast.LocalName(Name.LocalVar(text)), _) => 
                    SymTab.InstanceField(Name.Member(csym.name, text)) :: varMembers
                case _ =>
                    varMembers
            }
        }
        
        csym.VarMembers.v = cdecl.members.foldLeft(List[SymTab.Entry]())(addVarMembersFromMember)
        csym.VarMembers.v = addVarMembersFromParam(csym.VarMembers.v, cdecl.pattern)
    }
    
}