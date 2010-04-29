package harmonic.compiler

import java.io.File

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Map

import Ast.{Parse => in}
import Ast.{Resolve => out}
import Util._
import SymTab.extendedMap

/** Common code shared by `ResolveHeader` and `ResolveBody`. 
  * Contains methods to resolve the shortened name of a class
  * (which may omit the package) to the full, absolute name 
  * including the package. */
abstract class Resolve(state: CompilationState, compUnit: in.CompUnit) {
    
    // Imports are considered in reverse order.  Certain
    // default imports are added to the head of the list.
    val allImports = (
        in.ImportAll(Ast.AbsName(Name.QualRoot)) ::
        in.ImportAll(Ast.AbsName(Name.Package("java.lang"))) ::
        in.ImportAll(Ast.AbsName(Name.Package("harmonic.lang"))) ::
        in.ImportAll(compUnit.pkg) ::
        compUnit.imports
    ).reverse
    
    private[this] def resolveAgainst(ctxName: Name.Qual, nm: String) = {
        val className = Name.Class(ctxName, nm)
        if(loadedOrLoadable(className)) {
            Some(className)
        } else {
            ctxName match {
                case pkgName: Name.Package => 
                    Some(Name.Subpackage(pkgName, nm))
                case _ => 
                    None
            }                    
        }            
    }
    
    /** Given a relative name (represented in list form, like 
      * `List(String, lang, java)`), match it against the
      * imports to come up with a list of possible qualified names. 
      * Symbols for any ClassNames found in the resulting list exist. */
    private[this] def resolveRelList(relList: List[String]): List[Name.Qual] = relList match {
        case Nil => List(Name.Root) // not a valid input, really.
        
        case List(Name) => {
            allImports.flatMap { 
                case in.ImportOne(from, in.RelBase(Name)) => Some(from)
                case in.ImportOne(_, _) => None
                case in.ImportAll(Ast.PackageName(pkg)) => resolveAgainst(pkg, Name)
            }
        }
        
        case name :: context => {
            val expansions = resolveRelList(context)
            expansions.flatMap(resolveAgainst(_, name))            
        }
    }
    
    def resolveToClass(pos: Position, relList: List[String]): Option[Name.Class] = {
        val expansions = resolveRelList(relList)
        val result = expansions.find(_.isClassName)
        if(!result.isDefined) {
            state.reporter.report(relName.pos, "cannot.resolve", relName.toString)
        }
        result
    }

    /** Converts a relative name like `a.b.c` to a list like `List("c", "b", "a") */
    def relNameToRelList(relName: in.RelName): List[String] = relName match {
        case in.RelBase(name) => List(name)
        case in.RelDot(context, name) => name :: relNameToRelList(context)
    }
    
    def resolveName(relName: in.RelName) = {
        val className = resolveToClass(relNameToRelList(relName)).getOrElse(Name.ObjectQual)
        withPosOf(relName, Ast.ClassName(className))
    }

}