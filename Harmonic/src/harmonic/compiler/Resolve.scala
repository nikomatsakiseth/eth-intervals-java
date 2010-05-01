package harmonic.compiler

import java.io.File

import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position

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
    
    protected[this] def resolveAgainstPackage(pkgName: Name.Package, nm: String): Name.Qual = {
        val className = Name.Class(pkgName, nm)
        if(state.loadedOrLoadable(className)) {
            className
        } else {
            Name.Subpackage(pkgName, nm)
        }
    }
    
    protected[this] def resolveAgainstClass(clsName: Name.Class, nm: String): Option[Name.Class] = {
        val className = Name.Class(clsName, nm)
        if(state.loadedOrLoadable(className)) {
            Some(className)
        } else {
            None
        }
    }
    
    protected[this] def resolveAgainst(ctxName: Name.Qual, nm: String) = {
        ctxName match {
            case ctxName: Name.Class => resolveAgainstClass(ctxName, nm)
            case ctxName: Name.Package => Some(resolveAgainstPackage(ctxName, nm))
        }
    }
    
    // Internal representation of imports:
    sealed abstract class Import
    case class ImportAll(qualName: Name.Qual)
    case class ImportOne(qualName: Name.Qual, as: String)
    
    def resolveAbsToQual(relName: in.RelName): Option[Name.Qual] = {
        relName match {
            case in.RelBase(name) => 
                resolveAgainst(Name.Root, name)
            case in.RelDot(context, name) => 
                resolveAbsToQual(context) match {
                    case Some(q) => resolveAgainst(q, name)
                    case None => None
                }
        }        
    }
    
    def resolveImport(imp: in.ImportDecl) = {        
        imp match {
            case in.ImportAll(pkg) =>
                resolveAbsToQual(pkg).map(ImportAll(_))

            case in.ImportOne(from, in.RelBase(name)) =>
                resolveAbsToQual(from).map(ImportOne(_, name))
        }
    }
    
    // Imports are considered in reverse order.  Certain
    // default imports are added to the head of the list.
    val allImports = (
        ImportAll(Name.Root) ::
        ImportAll(Name.Package("java.lang")) ::
        ImportAll(Name.Package("harmonic.lang")) ::
        ImportAll(compUnit.pkg.name) ::
        compUnit.imports.flatMap(resolveImport)
    ).reverse

    /** Given a relative name (represented in list form, like 
      * `List(String, lang, java)`), match it against the
      * imports to come up with a list of possible qualified names. 
      * Symbols for any ClassNames found in the resulting list exist. */
    private[this] def resolveRelList(relList: List[String]): List[Name.Qual] = relList match {
        case Nil => List(Name.Root)
        
        case List(name) => {
            allImports.flatMap { 
                case ImportOne(from, to) if (to == name) => Some(from)
                case ImportOne(_, _) => None
                case ImportAll(pkg) => resolveAgainst(pkg, name)
            }
        }
        
        case name :: context => {
            val expansions = resolveRelList(context)
            expansions.flatMap(resolveAgainst(_, name))            
        }
    }
    
    def resolveToClass(pos: Position, relList: List[String]): Option[Name.Class] = {
        val expansions = resolveRelList(relList)
        val result = expansions.firstSome(_.asClassName)
        if(!result.isDefined) {
            Error.CannotResolve(relList.reverse.mkString(".")).report(state, pos)
        }
        result
    }

    /** Converts a relative name like `a.b.c` to a list like `List("c", "b", "a") */
    def relNameToRelList(relName: in.RelName): List[String] = relName match {
        case in.RelBase(name) => List(name)
        case in.RelDot(context, name) => name :: relNameToRelList(context)
    }
    
    def resolveName(relName: in.RelName) = withPosOf(relName, {
        val className = resolveToClass(relName.pos, relNameToRelList(relName)).getOrElse(Name.ObjectClass)
        Ast.ClassName(className)
    })

}