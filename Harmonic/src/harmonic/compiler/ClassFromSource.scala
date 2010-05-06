package harmonic.compiler

import scala.collection.mutable
import scala.util.parsing.input.Position
import ch.ethz.intervals._
import Util._

class ClassFromSource(
    name: Name.Class,
    global: Global,
    val pos: Position
) extends ClassSymbol(name, global) {

    def internalImplName = name.internalName + ByteCode.implSuffix
    
    def isNamed(aName: Name.Qual) = (name == aName)
    
    /** Array containing the intervals for each of
      * the passes defined in the class `Pass`. 
      * Initialized in Global.createSymbols() and used
      * read-only thereafter. */
    var intervals: Array[Interval] = null
    
    def optInterval(idx: Int) = Some(intervals(idx))
    
    // ___ Guarded Data _____________________________________________________
    
    class GuardedBy[T](pass: Int) {
        private[this] var value: Option[T] = None
        
        def get: T = {
            assert(Intervals.checkReadable(intervals(pass)))
            value.get
        }

        def set(v: T) = {
            assert(Intervals.checkWritable(intervals(pass)))
            value = Some(v)
        }
    }

    // ___ Computed by Pass.Header __________________________________________

    var superClassNames: List[Name.Class] = Nil
    
    var varMembers: List[SymTab.Entry] = Nil
    
    // ___ Computed by Pass.Body ____________________________________________
    
    /** Class declaration with names fully resolved. */
    var resolvedSource: Ast.Resolve.ClassDecl = null
    
    def modifiers = {
        Modifier.forResolvedAnnotations(resolvedSource.annotations)            
    }
        
    // ___ Computed by Pass.Create __________________________________________
    
    /** Lowered version of class parameter */
    var classParam: Ast.Lower.Param = null
    
    /** Environment immediately within the class body */
    var classEnv: Env = null
    
    /** Method symbol for constructor, once defined. */
    var constructor: MethodSymbol = null
    
    /** List of class members being lowered. */
    var lowerMembers: List[LowerMember] = Nil
    
    def constructors = {
        intervals(Pass.Create).join()        
        List(constructor)
    }
    
    def methodsNamed(mthdName: Name.Method) = {
        intervals(Pass.Create).join()
        lowerMembers.flatMap(_.toOptMethodSymbol(mthdName))
    }
    
    def fieldNamed(name: Name.Member) = {
        intervals(Pass.Create).join()
        lowerMembers.firstSome(_.toOptFieldSymbol(name)).ifNone {
            classParam.symbols.find(_.isNamed(name))                
        }
    }
    
    // ___ Computed by Pass.Merge ___________________________________________

    var loweredMethods: List[(MethodSymbol, Ast.Lower.MethodDecl)] = Nil
    
    var loweredFields: List[(VarSymbol.Field, Ast.Lower.FieldDecl)] = Nil
    
    var loweredSource: Ast.Lower.ClassDecl = null
    
    def allMethodSymbols: List[MethodSymbol] = loweredMethods.map(_._1)
    
    // ___ Computed by Pass.Gather __________________________________________
    
    /** A complete list of all versions of all methods offered by this class,
      * whether they are defined in this class or in a superclass. Populated 
      * by GatherOverrides for all classes being compiled and their supertypes. */
    var methodGroups: List[MethodGroup] = Nil
    
    def setMethodGroups(groups: List[MethodGroup]) {
        methodGroups = groups
    }

}