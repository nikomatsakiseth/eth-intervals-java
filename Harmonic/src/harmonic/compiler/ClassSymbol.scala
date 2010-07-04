package harmonic.compiler

import scala.collection.mutable
import scala.util.parsing.input.Position
import ch.ethz.intervals._
import com.smallcultfollowing.lathos.model.Context
import Util._

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
    
    // ___ Invoking causes header to be resolved ____________________________
    
    /** Names of any superclasses */
    def superClassNames: List[Name.Class]
    
    lazy val transitiveSuperClassSymbols: Set[ClassSymbol] = {
        val queued = new mutable.Queue[ClassSymbol]()
        val visited = new mutable.HashSet[ClassSymbol]()
        queued += this
        while(!queued.isEmpty) {
            val nextCsym = queued.dequeue()
            visited += nextCsym
            queued ++= nextCsym.superClassNames.map(global.csym).filterNot(visited)
        }
        visited.toSet                  
    }
    
    /** True if `this` is a subclass of `csym_sup` */
    def isSubclass(superCsym: ClassSymbol): Boolean = {
        (this == superCsym) || transitiveSuperClassSymbols(superCsym)
    }
    
    /** List of all "variable" members and what kind they are. 
      * Variable members include fields, ghosts, etc. The results 
      * of this method are used to populate the symbol tables
      * during resolution. */
    def varMembers: List[SymTab.Entry]
    
    // ___ Invoking causes MRO to be resolved _______________________________

    /** Method resolution order for this class.  First element in the
      * list is always `this`. */
    def mro: List[ClassSymbol]
    
    // ___ Invokable once body is resolved __________________________________
    //
    // Invoking these methods may result in lowering or in new classes being
    // loaded.
    
    /** List of all constructors for this class */
    def constructors: List[MethodSymbol]
    
    /** Symbols for methods defined on this class (not superclasses) 
      * with the given name.  May trigger lowering or other processing. */
    def methodsNamed(name: Name.Method): List[MethodSymbol]
    
    /** Symbols for fields defined on this class (not superclasses)
      * with the given name.  May trigger lowering or other processing. */
    def fieldNamed(name: Name.Member): Option[VarSymbol.Field]
    
    // ___ Invokable once type checking is complete _________________________
    //
    // The environment used within the class body.
    
    def checkEnv: Env
    
    // ___ Invokable once lowered ___________________________________________
    
    /** Symbols for all methods defined on this class but not superclasses. */
    def allMethodSymbols: List[MethodSymbol]
    
    /** Symbols for all methods defined on this class but not superclasses. */
    def allFieldSymbols: List[VarSymbol.Field]
    
    /** Symbols for all interval members defined on this class but not superclasses. */
    def allIntervalSymbols: List[VarSymbol.Field]
    
}