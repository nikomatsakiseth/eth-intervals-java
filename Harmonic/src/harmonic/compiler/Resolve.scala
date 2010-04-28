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
    
    /** Resolves `Rn` to a list of possible expansions.
      * Any class names included in the list have been loaded. 
      * The expansions are ordered in list of preference. */
    def resolveRelName(relName: in.RelName): List[Name.Qual] = {
        def resolveAgainst(ctxName: Name.Qual, nm: String) = {
            val className = Name.Class(ctxName, nm)
            if(loadedOrLoadable(className)) 
                Some(className)
            else {
                ctxName match {
                    case pkgName: Name.Package => 
                        Some(Name.Subpackage(pkgName, nm))
                    case _ => 
                        None
                }                    
            }            
        }
        
        relName match {
            case in.RelDot(ctx, nm) => {
                val expansions = resolveRelName(ctx)
                expansions.flatMap(resolveAgainst(_, nm))
            }
            
            case Rb @ in.RelBase(nm) => {
                allImports.flatMap { 
                    case in.ImportOne(from, Rb) => Some(from)
                    case in.ImportOne(_, _) => None
                    case in.ImportAll(Ast.PackageName(pkg)) => resolveAgainst(pkg, nm)
                }
            }
        }
    }
    
    def resolveToClass(relName: in.RelName): Name.Class = {
        val expansions = resolveRelName(relName)
        val optResult = expansions.firstSome {
            case className: Name.Class => className
            case _: Name.Package => None
        }
        optResult.getOrElse {
            state.reporter.report(relName.pos, "cannot.resolve", relName.toString)
            Name.ObjectQual
        }
    }

    def resolveName(relName: in.RelName) = 
        withPosOf(relName, Ast.ClassName(resolveToClass(relName)))

}