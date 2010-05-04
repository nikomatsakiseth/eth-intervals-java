package harmonic.compiler

import scala.collection.mutable
import scala.util.parsing.input.Position
import ch.ethz.intervals.Interval

object ClassSymbol {
    // Pre-defined interval names
    // (Plug-ins may define additional intervals)
    val Header = "Header"
    val Body ="Body"
    val Lower ="Lower"
    val Create = "Create"
    val Members = "Members"
    val Merge = "Merge"
    val ByteCode = "ByteCode"    
}

abstract class ClassSymbol(
    val name: Name.Class,
    global: Global
) extends Symbol {
    
    def interval(name: String): Option[Interval]
    
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
    def toType: Type.Class = Type.Class(name, List())
    
    // ___ Invokable once header is resolved ________________________________
    
    /** Names of any superclasses */
    def superClassNames: List[Name.Class]
    
    /** True if `this` is a subclass of `csym_sup` */
    def isSubclass(superCsym: ClassSymbol) = {
        (this == superCsym) || {
            val queued = new mutable.Queue[ClassSymbol]()
            val visited = new mutable.HashSet[ClassSymbol]()
            queued += this
            while(!queued.isEmpty && !visited(superCsym)) {
                val nextCsym = queued.dequeue()
                visited += nextCsym
                queued ++= nextCsym.superClassNames.map(global.csym).filterNot(visited)
            }
            visited(superCsym)                
        }
    }
    
    /** List of all "variable" members and what kind they are. 
      * Variable members include fields, ghosts, etc. The results 
      * of this method are used to populate the symbol tables
      * during resolution. */
    def varMembers: List[SymTab.Entry]
    
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
    
    // ___ Invokable once lowered ___________________________________________
    
    /** Symbols for all methods defined on this class but not superclasses. */
    def allMethodSymbols: List[MethodSymbol]
    
}