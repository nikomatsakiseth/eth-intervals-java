package harmonic.compiler

import ch.ethz.intervals._
import com.smallcultfollowing.lathos._
import com.smallcultfollowing.lathos.Lathos
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition
import scala.collection.mutable
import Util._

// ___ Supertypes _______________________________________________________

// Either a field, ghost, or a method
sealed trait BeanSymbol extends Symbol

// Field or a ghost
sealed trait FieldLikeSymbol extends BeanSymbol

// ___ Methods __________________________________________________________

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
)(implicit global: Global) extends BeanSymbol with DebugPage {
    override def toString = "MethodSymbol(%s.%s, %x)".format(className, name, System.identityHashCode(this))
    
    def id = MethodId(className, name, msig)
    
    def isFromClassNamed(aName: Name.Qual) = (className == aName)
    
    def isNamed(aName: Name.Method) = (name == aName)
    
    def substForFlatArgs(flatArgs: List[SPath.Typed]) = {
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

// ___ Ghosts ___________________________________________________________

class GhostSymbol(
    val pos: Position, 
    
    /** The name of the ghost. */
    val name: Name.Member,
    
    /** All objects bound to this ghost must be instance of this class */
    val bound: Name.Class
) extends FieldLikeSymbol with DebugPage {
    override def toString = "Ghost(%s, %x)".format(name, System.identityHashCode(this))
    
    val modifiers = Modifier.Set.empty
    
    def isNamed(aName: Name.Member) = (name == aName)
}

// ___ Variables and fields _____________________________________________

object VarSymbol {
    type Any = VarSymbol[Name.Var]
    
    class Field(
        val pos: Position,
        val modifiers: Modifier.Set,
        val name: Name.Member,
        val ty: Type,
        val kind: FieldKind,
        val elaborate: Interval        
    )(implicit global: Global) extends VarSymbol[Name.Member] with FieldLikeSymbol {
        // ___ Elaborate Phase __________________________________________________
        //
        // After the symbol is constructed, there is a later phase that 
        // computes the guard path.

        val GuardPath = new GuardedBy[Path.Ref](elaborate)
        def guardPath = GuardPath.v
    }
    
    def errorField(name: Name.Member, optExpTy: Option[Type])(implicit global: Global) = {
        val ty = optExpTy.getOrElse(Type.Null)
        inlineInterval("errorField") { elab =>
            val fsym = new Field(NoPosition, Modifier.Set.empty, name, ty, FieldKind.Harmonic, elab) {
                override def isError = true
            }
            fsym.GuardPath.v = Path.Final
            fsym
        }
    }
    
    class Local(
        val pos: Position,
        val modifiers: Modifier.Set,
        val name: Name.LocalVar,
        val ty: Type
    ) extends VarSymbol[Name.LocalVar] {
        def toPath = Path.Local(name)
        def toSPath = SPath.Local(this)
        
        def guardPath: Path.Ref = {
            if(modifiers.isNotMutable) Path.Final
            else Path.Method // TODO configurable
        }
    }
    
    def errorLocal(name: Name.LocalVar, optExpTy: Option[Type]) = {
        val ty = optExpTy.getOrElse(Type.Null)
        new Local(NoPosition, Modifier.Set.empty, name, ty) {
            override def isError = true
        }
    }
}

abstract class VarSymbol[+N <: Name.Var] extends Symbol with Page {
    val modifiers: Modifier.Set
    val name: N
    val ty: Type
    
    override def toString = "%s(%s, %x)".format(
        getClass.getSimpleName,
        name, 
        System.identityHashCode(this)
    )
    
    def isNamed(aName: Name.Var) = (name == aName)
    
    def guardPath: Path.Ref
    
    // ___ Page interface ___________________________________________________
    
    override def getId = "VarSymbol[%s]".format(System.identityHashCode(this))
    
    override def getParent = null
    
    override def addContent(content: PageContent) = throw new UnsupportedOperationException()
    
    override def renderInLine(out: Output): Unit = {
        Lathos.renderInLine(this, out)
    }
    
    override def renderInPage(out: Output): Unit = {
        out.startPage(this)
        
        out.startTable
        
        out.row("name", name)
        out.row("class", getClass.getSimpleName)
        out.row("ty", ty)
        out.row("pos", pos)
        
        out.endTable
        
        out.endPage(this)
    }
    
}