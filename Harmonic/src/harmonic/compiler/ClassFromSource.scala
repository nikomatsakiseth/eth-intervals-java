package harmonic.compiler

import scala.collection.mutable
import ch.ethz.intervals.Interval
import Util._

class ClassFromSource(
    name: Name.Class,
    global: Global
) extends ClassSymbol(name, global) {

    def internalImplName = name.internalName + ByteCode.implSuffix
    
    private[this] var errorReports = 0
    
    def addErrorReport() = synchronized {
        errorReports += 1
    }
    
    private[this] val intervals = new mutable.ListMap[String, Interval]()
    
    def interval(name: String) = synchronized {
        intervals.get(name)
    }
    
    def addInterval(name: String)(inter: Interval): Interval = synchronized {
        intervals(name) = inter
        inter    
    }
    
    def isNamed(aName: Name.Qual) = (name == aName)
    
    def pos = resolvedSource.pos

    // ___ Computed by Header Interval ______________________________________

    var superClassNames: List[Name.Class] = Nil
    
    var varMembers: List[SymTab.Entry] = Nil
    
    // ___ Computed by Body Interval ________________________________________
    
    /** Class declaration with names fully resolved. */
    var resolvedSource: Ast.Resolve.ClassDecl = null
    
    def modifiers = {
        Modifier.forResolvedAnnotations(resolvedSource.annotations)            
    }
        
    // ___ Computed by Create Interval ______________________________________
    
    /** Lowered version of class parameter */
    var classParam: Ast.Lower.Param = null
    
    /** Environment immediately within the class body */
    var classEnv: Env = null
    
    /** Method symbol for constructor, once defined. */
    var constructor: MethodSymbol = null
    
    /** List of class members being lowered. */
    var lowerMembers: List[LowerMember] = Nil
    
    def constructors = List(constructor)
    
    // ___ Computed by Subintervals of Members Interval _____________________

    def methodsNamed(mthdName: Name.Method) = {
        interval(ClassSymbol.Create).foreach(_.join())
        lowerMembers.flatMap(_.toOptMethodSymbol(mthdName))
    }
    
    def fieldNamed(name: Name.Member) = {
        interval(ClassSymbol.Create).foreach(_.join())
        lowerMembers.firstSome(_.toOptFieldSymbol(name))        
    }
    
    // ___ Computed by Merge Interval _______________________________________

    var loweredMethods: List[(MethodSymbol, Ast.Lower.MethodDecl)] = Nil
    
    def allMethodSymbols: List[MethodSymbol] = loweredMethods.map(_._1)
    
    var loweredSource: Ast.Lower.ClassDecl = null
    
    // ___ Computed by Bytecode Interval ____________________________________
    
    /** A complete list of all versions of all methods offered by this class,
      * whether they are defined in this class or in a superclass. Populated 
      * by GatherOverrides for all classes being compiled and their supertypes. */
    var methodGroups: List[MethodGroup] = Nil
    
    def setMethodGroups(groups: List[MethodGroup]) {
        methodGroups = groups
    }

}