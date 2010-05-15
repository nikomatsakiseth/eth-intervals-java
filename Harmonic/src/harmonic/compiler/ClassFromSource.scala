package harmonic.compiler

import scala.collection.mutable
import scala.util.parsing.input.Position
import ch.ethz.intervals._
import Util._

class ClassFromSource(
    name: Name.Class,
    global: Global,
    compUnit: Ast.Parse.CompUnit,
    parseCdecl: Ast.Parse.ClassDecl
) extends ClassSymbol(name, global) {
    import global.master
    
    def pos = parseCdecl.pos

    def internalImplName = name.internalName + ByteCode.implSuffix
    
    def isNamed(aName: Name.Qual) = (name == aName)
    
    // ___ Interval Creation ________________________________________________
    //
    // For a class being loaded from source, this is the structure:
    //
    // header -> body -> | lower --------------------- | ---> gather -> byteCode
    //      ^             \                           /^   ^
    //      |              create -> members -> (merge)|   |
    //   header                     \        /         |   | 
    //  (supers)                    (member0)      lower   |  
    //                                 ...        (supers) |   
    //                              (memberN)           gather         
    //                                                 (supers)
    // 
    // header: determines the list of members, names of superclasses.
    //
    // body: resolves members in the body to absolute class names, etc.
    //
    // lower: summary interval for the job of reducing IR to the simpler form
    // - create: creates intervals for each member and the merge interval
    // - members: summary interval for the intervals that lower each indiv. member
    // - merge: merges all the lowered members into a lowered class declaration.
    // 
    // gather: checks the overrides between methods
    //
    // byteCode: emits the byte code from the lowered form
    //
    // Those intervals whose names are listed in parentheses are created by
    // the create interval and are not listed in the intervals array.
    //
    // The intervals labeled (supers) are the corresponding intervals from
    // the supertypes of this class (if any).  Those dependencies are added
    // during the header pass (see `ResolveHeader`).

    val header: Interval = master.subinterval(
        name = "%s.Header".format(name)
    ) { 
        _ => ResolveHeader(global, compUnit).resolveClassHeader(this, parseCdecl)                        
    }
    
    val body: Interval = master.subinterval(
        name = "%s.Body".format(name),
        after = List(header.end)
    ) { 
        _ => ResolveBody(global, compUnit).resolveClassBody(this, parseCdecl)
    }

    val lower: Interval = master.subinterval(
        name = "%s.Lower".format(name),
        after = List(body.end)
    ) { 
        _ => () // Lower is really just a placeholder.
    }
    
    val create: Interval = master.subinterval(
        name = "%s.Create".format(name),
        during = List(lower)
    ) {
        _ => Create(global).createMemberIntervals(this)
    }
    
    val members: Interval = master.subinterval(
        name = "%s.Members".format(name),
        during = List(lower), 
        after = List(create.end)
    ) { 
        _ => ()
    }
    
    val merge: Interval = master.subinterval(
        name = "%s.Merge".format(name),
        during = List(lower), 
        after = List(members.end)
    ) { 
        _ => Merge(global).mergeMemberIntervals(this)
    }
    
    val gather: Interval = master.subinterval(
        name = "%s.Gather".format(name),
        after = List(lower.end)
    ) {
        _ => Gather(global).forSym(this)
    }

    val byteCode: Interval = master.subinterval(
        name = "%s.ByteCode".format(name),
        after = List(gather.end)
    ) { 
        _ => if(!global.reporter.hasErrors) ByteCode(global).writeClassSymbol(this)
    }
    
    private[this] val intervals = Array(header, body,  lower, create, members, merge, gather, byteCode)
    intervals.foreach(_.schedule())
    
    def optInterval(idx: Int) = Some(intervals(idx))
    
    // ___ Guarded Data _____________________________________________________
    
    class GuardedBy[T](pass: Interval) {
        private[this] var value: Option[T] = None
        
        def join = {
            pass.join()
            v
        }
        
        def v: T = {
            assert(Intervals.checkReadable(pass))
            value.get
        }
        
        def v_=(v: T) = {
            assert(Intervals.checkWritable(pass))
            value = Some(v)
        }
    }

    // ___ Computed by Pass.Header __________________________________________

    val SuperClassNames = new GuardedBy[List[Name.Class]](header)
    def superClassNames = SuperClassNames.join

    val VarMembers = new GuardedBy[List[SymTab.Entry]](header)
    def varMembers = VarMembers.join
    
    // ___ Computed by Pass.Body ____________________________________________
    
    val ResolvedSource = new GuardedBy[Ast.Resolve.ClassDecl](body)
    def resolvedSource = ResolvedSource.v
    
    def modifiers = {
        Modifier.forResolvedAnnotations(resolvedSource.annotations)            
    }
        
    // ___ Computed by Pass.Create __________________________________________
    
    /** Lowered version of class parameter */
    val ClassParam = new GuardedBy[Ast.Lower.Param[VarSymbol.Field]](create)    
    def classParam = ClassParam.v
    
    /** Environment immediately within the class body */
    val ClassEnv = new GuardedBy[Env](create)
    def classEnv = ClassEnv.v
    
    /** Method symbol for constructor, once defined. */
    val Constructor = new GuardedBy[MethodSymbol](create)
    def constructor = Constructor.v
    
    /** List of class members being lowered. */
    val LowerMembers = new GuardedBy[List[LowerMember]](create)
    def lowerMembers = LowerMembers.v
    
    def constructors = {
        List(Constructor.join)
    }
    
    def methodsNamed(mthdName: Name.Method) = {
        LowerMembers.join.flatMap(_.toOptMethodSymbol(mthdName))
    }
    
    def fieldNamed(name: Name.Member) = {
        LowerMembers.join.firstSome(_.toOptFieldSymbol(name)).ifNone {
            val fsyms: List[VarSymbol.Field] = classParam.symbols
            fsyms.find(_.isNamed(name))
        }
    }
    
    // ___ Computed by Pass.Merge ___________________________________________

    val LoweredMethods = new GuardedBy[List[(MethodSymbol, Ast.Lower.MethodDecl)]](merge)
    def loweredMethods = LoweredMethods.v

    val LoweredSource = new GuardedBy[Ast.Lower.ClassDecl](merge)
    def loweredSource = LoweredSource.v

    def allMethodSymbols: List[MethodSymbol] = loweredMethods.map(_._1)

    val AllFieldSymbols = new GuardedBy[List[VarSymbol.Field]](merge)
    def allFieldSymbols = AllFieldSymbols.v
    
    // ___ Computed by Pass.Gather __________________________________________
    
    /** A complete list of all versions of all methods offered by this class,
      * whether they are defined in this class or in a superclass. Populated 
      * by GatherOverrides for all classes being compiled and their supertypes. */
    val MethodGroups = new GuardedBy[List[MethodGroup]](gather)
    def methodGroups = MethodGroups.v

    val ExtendedClasses = new GuardedBy[List[(Ast.Lower.ExtendsDecl, List[Path.Typed])]](gather)    
    def extendedClasses = ExtendedClasses.v
    
    def setMethodGroups(groups: List[MethodGroup]) {
        MethodGroups.v = groups
    }

}