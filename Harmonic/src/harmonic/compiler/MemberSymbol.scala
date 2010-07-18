package harmonic.compiler

import ch.ethz.intervals._
import com.smallcultfollowing.lathos._
import com.smallcultfollowing.lathos.Lathos
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition
import scala.collection.mutable
import java.lang.reflect
import Util._

// ___ Supertypes _______________________________________________________

// Either a field, ghost, or a method
sealed trait BeanSymbol extends Symbol with DebugPage

// Field or a ghost
sealed trait FieldLikeSymbol extends BeanSymbol

// ___ Methods __________________________________________________________

object MethodSymbol {
    
    abstract class ErrorMethod(methodId: MethodId)(implicit global: Global)
    extends MethodSymbol {
        val className: Name.Class = methodId.className
        val name: Name.Method = methodId.methodName
        val pos = InterPosition.forClassNamed(methodId.className)
        val modifiers = Modifier.Set.empty
        val kind = MethodKind.ErrorMethod
        val msig = MethodSignature(Type.Null, Pattern.makeRefs(methodId.patterns))
        val guardPath = Path.RacyGuard
        val requirements = Nil
        val ensures = Nil
        
        override def isError: Boolean = true
    }

    def error(methodId: MethodId.Static)(implicit global: Global): StaticMethodSymbol = {
        new ErrorMethod(methodId) with StaticMethodSymbol
    }
    
    def error(methodId: MethodId.Virtual)(implicit global: Global): VirtualMethodSymbol = {
        new ErrorMethod(methodId) with VirtualMethodSymbol
    }
    
    def error(name: Name.Method, className: Name.Class)(implicit global: Global): VirtualMethodSymbol = {
        val patterns = name.parts.map(_ => Pattern.AnonVar(Type.Top))
        val methodId = MethodId.Virtual(className, name, patterns)
        error(methodId)
    }
    
}

trait MethodSymbol extends Symbol {
    val pos: Position
    val modifiers: Modifier.Set
    val kind: MethodKind
    val className: Name.Class
    val name: Name.Method
    val msig: MethodSignature[Pattern.Ref]
    val id: MethodId
    def guardPath: Path
    def requirements: List[inference.Fact]
    def ensures: List[inference.Fact]
    
    def ifStatic: Option[StaticMethodSymbol]
    def ifVirtual: Option[VirtualMethodSymbol]
    
    def isFromClassNamed(aName: Name.Class) = (className is aName)
    def isNamed(aName: Name.Method) = (name is aName)
    def ifId(id: MethodId.Static): Option[StaticMethodSymbol]
    def ifId(id: MethodId.Virtual): Option[VirtualMethodSymbol]
    
    def substForFlatSPaths(flatArgs: List[SPath[Phantasmal]]) = {
        msig.parameterPatterns.flatMap(_.varNames).zip(flatArgs).foldLeft(Subst.empty) {
            case (s, (x, tp)) => s + (x.toPath -> tp.toPath)
        }
    }
    
    override def toString = "%s(%s)".format(getClass.getSimpleName, id)
}

trait StaticMethodSymbol extends MethodSymbol {
    lazy val id: MethodId.Static = MethodId.Static(className, name, msig.toAnon.parameterPatterns)
    
    override def ifId(anId: MethodId.Static) = (id is anId).toOption(this)
    override def ifId(anId: MethodId.Virtual) = None
    
    override def ifStatic: Option[StaticMethodSymbol] = Some(this)
    override def ifVirtual: Option[VirtualMethodSymbol] = None
    
    def toReflectedJavaMethod: Option[reflect.Method] = None
}

trait VirtualMethodSymbol extends MethodSymbol with BeanSymbol {
    lazy val id: MethodId.Virtual = MethodId.Virtual(className, name, msig.toAnon.parameterPatterns)
    
    override def ifId(anId: MethodId.Static) = None
    override def ifId(anId: MethodId.Virtual) = (id is anId).toOption(this)
    
    override def ifStatic: Option[StaticMethodSymbol] = None
    override def ifVirtual: Option[VirtualMethodSymbol] = Some(this)
}

// ___ Ghosts ___________________________________________________________

class GhostSymbol(
    val pos: Position, 
    
    /** The name of the ghost. */
    val name: Name.Member,
    
    /** All objects bound to this ghost must be instance of this class */
    val bound: Name.Class
) extends FieldLikeSymbol {
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

        val GuardPath = new GuardedBy[Path](elaborate)
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
        
        def guardPath: Path = {
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

sealed trait VarSymbol[+N <: Name.Var] extends Symbol {
    val modifiers: Modifier.Set
    val name: N
    val ty: Type
    
    override def toString = "%s(%s, %x)".format(
        getClass.getSimpleName,
        name, 
        System.identityHashCode(this)
    )
    
    def isNamed(aName: Name.Var) = (name == aName)
    
    def guardPath: Path
    
}