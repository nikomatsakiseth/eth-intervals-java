package harmonic.compiler

import java.io.File

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Map

import Ast.{Parse => in}
import Ast.{Resolve => out}
import Util._
import SymTab.extendedMap

/** Header resolution resolves only those parts needed to resolve the
  * rest of the body.  Before we can resolve a class body, we must first 
  * ensure that its headers and those of its superclasses (transitively) have 
  * been resolved. */
case class ResolveHeader(state: State, compUnit: in.CompUnit) 
extends Resolve(state, compUnit) 
{
    
    def resolveClassHeader(
        csym: Symbol.ClassFromSource,
        cdecl: in.ClassDecl
    ) {
        csym.superClassNames = cdecl.superClasses.map(resolveName).map(_.name)
        val superCsyms = csym.superClassNames.map(state.classes)
        val superHeaderPasses = superCsyms.flatMap(_.resolveHeader)
        csym.resolveHeaderClosure.foreach(_.addDependencies(superHeaderPasses))
        
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