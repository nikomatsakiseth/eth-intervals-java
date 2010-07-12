package harmonic.compiler

import ch.ethz.intervals._
import com.smallcultfollowing.lathos._
import com.smallcultfollowing.lathos.Lathos
import scala.util.parsing.input.Position
import scala.collection.mutable
import Util._

object MethodSymbol {
    
    def error(methodId: MethodId): MethodSymbol = {
        throw new RuntimeException("TODO--gin up a fake methodsymbol from methodid")
        //error(methodId.methodName, methodId.className, methodId.msig.parameterPatterns)
    }
    
    def error(name: Name.Method, className: Name.Class, patterns: List[Pattern.Ref])(implicit global: Global): MethodSymbol = {
        inlineInterval("Error %s.%s".format(className, name)) { inter =>
            new MethodSymbol(
                pos       = InterPosition.forClassNamed(className),
                modifiers = Modifier.Set.empty,
                kind      = MethodKind.ErrorMethod, 
                className = className, 
                name      = name, 
                elaborate = inter,
                msig      = MethodSignature(Type.Null, patterns)
            ) {
                override def isError = true
            }            
        }
    }

    def error(name: Name.Method, className: Name.Class)(implicit global: Global): MethodSymbol = {
        val parameterPatterns = name.parts.zipWithIndex.map { case (_, i) => 
            Pattern.Var(Name.LocalVar("arg%d".format(i)), Type.Top)
        }
        error(name, className, parameterPatterns)
    }
    
}

class MethodSymbol(
    val pos: Position,
    val modifiers: Modifier.Set,
    val kind: MethodKind,            /** Intrinsic, harmonic, java, etc. */
    val className: Name.Class,         /** Class in which the method is defined. */
    val name: Name.Method,           /** Name of the method. */
    val msig: MethodSignature[Pattern.Ref],
    val elaborate: Interval
)(implicit global: Global) extends Symbol with DebugPage {
    override def toString = "MethodSymbol(%s.%s, %x)".format(className, name, System.identityHashCode(this))
    
    def id = MethodId(className, name, msig)
    
    def isFromClassNamed(aName: Name.Qual) = (className == aName)
    
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
    
    val Requirements = new GuardedBy[List[inference.Fact]](elaborate)
    def requirements = Requirements.v
    
    val Ensures = new GuardedBy[List[inference.Fact]](elaborate)
    def ensures = Ensures.v
    
}
