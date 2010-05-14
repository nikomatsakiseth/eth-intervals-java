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
        
        def v: T = {
            assert(Intervals.checkReadable(intervals(pass)))
            value.get
        }
        
        def v_=(v: T) = {
            assert(Intervals.checkWritable(intervals(pass)))
            value = Some(v)
        }
    }

    // ___ Computed by Pass.Header __________________________________________

    val SuperClassNames = new GuardedBy[List[Name.Class]](Pass.Header)
    def superClassNames = SuperClassNames.v

    val VarMembers = new GuardedBy[List[SymTab.Entry]](Pass.Header)
    def varMembers = VarMembers.v
    
    // ___ Computed by Pass.Body ____________________________________________
    
    val ResolvedSource = new GuardedBy[Ast.Resolve.ClassDecl](Pass.Body)
    def resolvedSource = ResolvedSource.v
    
    def modifiers = {
        Modifier.forResolvedAnnotations(resolvedSource.annotations)            
    }
        
    // ___ Computed by Pass.Create __________________________________________
    
    /** Lowered version of class parameter */
    val ClassParam = new GuardedBy[Ast.Lower.Param[VarSymbol.Field]](Pass.Create)    
    def classParam = ClassParam.v
    
    /** Environment immediately within the class body */
    val ClassEnv = new GuardedBy[Env](Pass.Create)
    def classEnv = ClassEnv.v
    
    /** Method symbol for constructor, once defined. */
    val Constructor = new GuardedBy[MethodSymbol](Pass.Create)
    def constructor = Constructor.v
    
    /** List of class members being lowered. */
    val LowerMembers = new GuardedBy[List[LowerMember]](Pass.Create)
    def lowerMembers = LowerMembers.v
    
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
            val fsyms: List[VarSymbol.Field] = classParam.symbols
            fsyms.find(_.isNamed(name))
        }
    }
    
    // ___ Computed by Pass.Merge ___________________________________________

    val LoweredMethods = new GuardedBy[List[(MethodSymbol, Ast.Lower.MethodDecl)]](Pass.Merge)
    def loweredMethods = LoweredMethods.v

    val LoweredSource = new GuardedBy[Ast.Lower.ClassDecl](Pass.Merge)
    def loweredSource = LoweredSource.v

    def allMethodSymbols: List[MethodSymbol] = loweredMethods.map(_._1)

    val AllFieldSymbols = new GuardedBy[List[VarSymbol.Field]](Pass.Merge)
    def allFieldSymbols = AllFieldSymbols.v
    
    // ___ Computed by Pass.Gather __________________________________________
    
    /** A complete list of all versions of all methods offered by this class,
      * whether they are defined in this class or in a superclass. Populated 
      * by GatherOverrides for all classes being compiled and their supertypes. */
    val MethodGroups = new GuardedBy[List[MethodGroup]](Pass.Gather)
    def methodGroups = MethodGroups.v

    val ExtendedClasses = new GuardedBy[List[(Ast.Lower.ExtendsDecl, List[Path.Typed])]](Pass.Gather)    
    def extendedClasses = ExtendedClasses.v
    
    def setMethodGroups(groups: List[MethodGroup]) {
        MethodGroups.v = groups
    }

}