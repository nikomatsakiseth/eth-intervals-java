package harmonic.compiler

import ch.ethz.intervals._
import com.smallcultfollowing.lathos.model._
import com.smallcultfollowing.lathos.model.{Util => LathosUtil}
import scala.util.parsing.input.Position
import scala.collection.mutable
import Util._

object MethodSymbol {
    
    def error(methodId: MethodId): MethodSymbol = {
        throw new RuntimeException("TODO--gin up a fake methodsymbol from methodid")
        //error(methodId.methodName, methodId.className, methodId.msig.parameterPatterns)
    }
    
    def error(name: Name.Method, clsName: Name.Class, patterns: List[Pattern.Ref]): MethodSymbol = {
        inlineInterval("Error %s.%s".format(clsName, name)) { inter =>
            new MethodSymbol(
                pos       = InterPosition.forClassNamed(clsName),
                modifiers = Modifier.Set.empty,
                kind      = MethodKind.ErrorMethod, 
                clsName   = clsName, 
                name      = name, 
                elaborate = inter,
                gather    = inter,
                msig      = MethodSignature(Type.Null, patterns)
            ) {
                override def isError = true
            }            
        }
    }

    def error(name: Name.Method, clsName: Name.Class): MethodSymbol = {
        val parameterPatterns = name.parts.zipWithIndex.map { case (_, i) => 
            Pattern.Var(Name.LocalVar("arg%d".format(i)), Type.Top)
        }
        error(name, clsName, parameterPatterns)
    }
    
}

class MethodSymbol(
    val pos: Position,
    val modifiers: Modifier.Set,
    val kind: MethodKind,            /** Intrinsic, harmonic, java, etc. */
    val clsName: Name.Class,         /** Class in which the method is defined. */
    val name: Name.Method,           /** Name of the method. */
    val msig: MethodSignature[Pattern.Ref],
    val elaborate: Interval,
    val gather: Interval
) extends Symbol with Page {
    override def toString = "MethodSymbol(%s.%s, %x)".format(clsName, name, System.identityHashCode(this))
    
    def id = MethodId(clsName, name, msig)
    
    def isFromClassNamed(aName: Name.Qual) = (clsName == aName)
    
    def isNamed(aName: Name.Method) = (name == aName)
    
    def substForFlatArgs(flatArgs: List[Path.Typed]) = {
        msig.parameterPatterns.flatMap(_.varNames).zip(flatArgs).foldLeft(Subst.empty) {
            case (s, (x, tp)) => s + (x.toPath -> tp.toPath)
        }
    }
    
    // ___ Elaborate Phase __________________________________________________
    //
    // After the symbol is constructed, there is a later phase that 
    // computes the "requirements" and "ensures" lists.  This is done in
    // a later phase to avoid problems when two methods reference one another
    // in the requirements lists (otherwise, one method would have to be 
    // fully constructed first, which would be impossible).
    
    val Requirements = new GuardedBy[List[Fact]](elaborate)
    def requirements = Requirements.v
    
    val Ensures = new GuardedBy[List[Fact]](elaborate)
    def ensures = Ensures.v
    
    // ___ Gather Phase _____________________________________________________
    //
    // List of methods overridden by this method.  The ordering is 
    // significant, because when super is invoked it will proceed to the
    // next implementation in the list.
    
    val Overrides = new GuardedBy[List[MethodSymbol]](gather)
    def overrides = Overrides.v
    
    // ___ Page interface ___________________________________________________
    
    override def getId = "MethodSymbol[%s]".format(System.identityHashCode(this))
    
    override def getParent = null
    
    override def addContent(content: PageContent) = throw new UnsupportedOperationException()
    
    override def renderInLine(out: Output): Unit = {
        LathosUtil.renderInLine(this, out)
    }
    
    override def renderInPage(out: Output): Unit = {
        out.startPage(this)
        
        out.startTable
        
        out.row("name", name)
        out.row("msig", msig)
        out.row("pos", pos)
        
        out.endTable
        
        out.endPage(this)
    }
    
}
