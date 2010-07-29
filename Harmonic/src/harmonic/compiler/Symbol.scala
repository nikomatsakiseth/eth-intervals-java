package harmonic.compiler

import scala.collection.mutable
import scala.util.parsing.input.Position
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition
import java.lang.reflect
import com.smallcultfollowing.lathos.Lathos
import ch.ethz.intervals._
import Util._

/*

Defines the various traits for different kinds
of symbols.  For the most part, does not include 
concrete symbol classes.

*/

// Any symbol at all
trait Symbol {
    def name: Name.Any
    def pos: Position
    def modifiers: Modifier.Set
    def isError: Boolean = false
}

// Either a field, ghost, or a method
sealed trait BeanSymbol extends Symbol with DebugPage

// Field or a ghost
sealed trait FieldLikeSymbol extends BeanSymbol

// ___ Class Symbols ___________________________________________________

abstract class ClassSymbol extends Symbol {
    val name: Name.Class
    val global: Global
    
    // ___ Intervals ________________________________________________________
    //
    // Every class symbol potentially has several intervals corresponding
    // to different processing phases.  For some class symbols, these intervals
    // may not actually do any work to speak of.  Note that the intervals
    // may be created lazily.
    
    def header: AsyncInterval
    def cmro: AsyncInterval    // "compute MRO"
    def body: AsyncInterval
    def lower: AsyncInterval
    def create: AsyncInterval
    def members: AsyncInterval
    def merge: AsyncInterval
    def envirate: AsyncInterval
    def check: AsyncInterval
    def gather: AsyncInterval
    def byteCode: AsyncInterval
    
    // ___ Invokable at any time ____________________________________________
    
    /** When a new instance of this class is created, what should we REALLY instantiate? */
    def internalImplName: String
    
    /** A position for reporting errors related to the class as a whole */
    def pos: Position
    
    /** Records the method groups derived by `GatherOverrides` for use
      * in byte code generation.  Has no effect for symbols loaded from
      * .class files or reflection. */
    def setMethodGroups(groups: List[MethodGroup]): Unit

    override def toString = "%s(%s, %x)".format(
        getClass.getSimpleName, name, System.identityHashCode(this)
    )
    
    /** Creates a `Type.Class` for the class defined by this symbol. */
    def toType: Type.Class = name.toType
    
    /** If this class is available via reflection, returns the Java `Class` instance. */
    def toReflectedJavaClass: Option[Class[_]] = None
    
    // ___ Invoking causes header to be joined ______________________________
    
    /** Names of any superclasses */
    def superClassNames: List[Name.Class]
    
    /** List of all "variable" members and what kind they are. 
      * Variable members include fields, ghosts, etc. The results 
      * of this method are used to populate the symbol tables
      * during resolution. */
    def varMembers: List[SymTab.Entry]
    
    // ___ Invoking causes cmro to be joined ________________________________

    /** Method resolution order for this class.  First element in the
      * list is always `this`. */
    def mro: List[ClassSymbol]
    
    /** True if `this` is a subclass of `csym_sup` */
    def isSubclass(superCsym: ClassSymbol): Boolean = {
        mro.contains(superCsym)
    }
    
    // ___ Invokable after `body` ___________________________________________
    //
    // Invoking these methods may result in lowering or in new classes being
    // loaded.
    
    /** List of all constructors for this class */
    def constructors: List[MethodSymbol]
    
    /** Supertypes of an instance of this class named `this`. */
    def superTypes: List[Type.Class]
    
    /** Symbols for methods defined on this class (not superclasses) 
      * with the given name.  May trigger lowering or other processing. */
    def methodsNamed(name: Name.Method): List[MethodSymbol]
    
    /** Symbols for fields defined on this class (not superclasses)
      * with the given name.  This includes intervals.  
      * May trigger lowering or other processing. */
    def fieldNamed(name: Name.Member): Option[FieldSymbol]
    
    /** Symbols for ghosts defined on this class. */
    def ghostNamed(name: Name.Member): Option[GhostSymbol]

    // ___ Invokable after `lower` __________________________________________
    
    /** Symbols for all methods defined on this class (but not superclasses). */
    def allMethodSymbols: List[MethodSymbol]
    
    /** Symbols for all fields defined on this class (but not superclasses). */
    def allFieldSymbols: List[FieldSymbol]
    
    /** Symbols for all interval members defined on this class (but not superclasses). */
    def allIntervalSymbols: List[FieldSymbol]
    
    /** Symbols for all ghosts defined on this class (but not superclasses). */
    def allGhostSymbols: List[GhostSymbol]
    
    // ___ Invokable after `check` __________________________________________
    //
    // The environment used within the class body.
    
    def checkEnv: Env
    
    // ___ Invokable after `gather` _________________________________________

    def AllOverrides: GuardedBy[Map[MethodSymbol, List[MethodSymbol]]]
    def allOverrides = AllOverrides.v
}

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
    
    class ErrorField(
        val name: Name.Member,
        val ty: Type
    ) extends FieldSymbol {
        override def pos = NoPosition
        override def modifiers = Modifier.Set.empty
        override def kind = FieldKind.Harmonic
        override val initializedTo = None
        override def guardPath = Path.Final
        override def isError = true
    }
    
    def errorField(name: Name.Member, optExpTy: Option[Type])(implicit global: Global) = {
        val ty = optExpTy.getOrElse(Type.Null)
        new ErrorField(name, ty)
    }
    
    class ErrorLocal(
        name: Name.LocalVar,
        ty: Type
    ) extends LocalSymbol(NoPosition, Modifier.Set.empty, name, ty) {
        override def isError = true
    }
        
    def errorLocal(name: Name.LocalVar, optExpTy: Option[Type]) = {
        val ty = optExpTy.getOrElse(Type.Null)
        new ErrorLocal(name, ty)
    }
}

sealed trait VarSymbol[+N <: Name.Var] extends Symbol {
    // Final:
    def modifiers: Modifier.Set
    def name: N
    
    // Typing phase:
    def ty: Type
    
    // Elaboration phase:
    def guardPath: Path
    
    override def toString = "%s(%s, %x)".format(
        getClass.getSimpleName,
        name, 
        System.identityHashCode(this)
    )
    
    def isNamed(aName: Name.Var) = (name == aName)
}

class LocalSymbol(
    val pos: Position,
    val modifiers: Modifier.Set,
    val name: Name.LocalVar,
    val ty: Type
) extends VarSymbol[Name.LocalVar] {
    def toPath = Path.Local(name)
    def toSPath = SPath.Local(this)
    def guardPath = {
        if(modifiers.isMutable)
            Path.Method
        else
            Path.Final
    }
}

trait FieldSymbol extends VarSymbol[Name.Member] with FieldLikeSymbol {
    def kind: FieldKind
    
    def initializedTo: Option[Path]
}
