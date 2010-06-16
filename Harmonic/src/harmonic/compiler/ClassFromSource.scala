package harmonic.compiler

import scala.collection.mutable
import scala.util.parsing.input.Position
import ch.ethz.intervals._
import Util._

class ClassFromSource(
    val name: Name.Class,
    val global: Global,
    compUnit: Ast.Parse.CompUnit,
    parseCdecl: Ast.Parse.ClassDecl
) extends ClassSymbol {
    import global.master
    
    def pos = parseCdecl.pos

    def internalImplName = name.internalName + ByteCode.implSuffix
    
    def isNamed(aName: Name.Qual) = (name == aName)
    
    // ___ Interval Creation ________________________________________________
    //
    // For a class being loaded from source, this is the structure:
    //
    // header -> body -> | lower --------------------- | ---> check ---> gather -> byteCode
    //      ^             \                           /^    ^         ^
    //      |              create -> members -> (merge)|    |         |
    //   header                     \        /         |    |         | 
    //  (supers)                    (member0)      lower    |         |  
    //                                 ...        (supers)  |         |   
    //                              (memberN)             check     gather         
    //                                                   (supers)  (supers)
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
    // check: perform type and race checking
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

    val header: AsyncInterval = master.subinterval(
        name = "%s.Header".format(name),
        // Hold off on scheduling header until class def'n is complete:
        schedule = false 
    ) { 
        _ => ResolveHeader(global, compUnit).resolveClassHeader(this, parseCdecl)                        
    }
    
    val body: AsyncInterval = master.subinterval(
        name = "%s.Body".format(name),
        after = List(header.getEnd)
    ) { 
        _ => ResolveBody(global, compUnit).resolveClassBody(this, parseCdecl)
    }

    val lower: AsyncInterval = master.subinterval(
        name = "%s.Lower".format(name),
        after = List(body.getEnd),
        // Scheduled explicitly so we can add
        // create/members/merge within
        schedule = false 
    ) { 
        _ => () // Lower is really just a placeholder.
    }
    
    val create: AsyncInterval = master.subinterval(
        name = "%s.Create".format(name),
        during = List(lower)
    ) {
        _ => Create(global).createMemberIntervals(this)
    }
    
    val members: AsyncInterval = master.subinterval(
        name = "%s.Members".format(name),
        during = List(lower), 
        after = List(create.getEnd)
    ) { 
        _ => ()
    }
    
    val merge: AsyncInterval = master.subinterval(
        name = "%s.Merge".format(name),
        during = List(lower), 
        after = List(members.getEnd)
    ) { 
        _ => Merge(global).mergeMemberIntervals(this)
    }
    
    val check: AsyncInterval = master.subinterval(
        name = "%s.Check".format(name),
        after = List(lower.getEnd)
    ) {
        _ => () // Check(global).classSymbol(this)
    }
    
    val gather: AsyncInterval = master.subinterval(
        name = "%s.Gather".format(name),
        after = List(check.getEnd)
    ) {
        _ => Gather(global).forSym(this)
    }

    val byteCode: AsyncInterval = master.subinterval(
        name = "%s.ByteCode".format(name),
        after = List(gather.getEnd)
    ) { 
        _ => if(!global.reporter.hasErrors) ByteCode(global).writeClassSymbol(this)
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

    val AllIntervalSymbols = new GuardedBy[List[VarSymbol.Field]](merge)
    def allIntervalSymbols = AllIntervalSymbols.v
    
    // ___ Computed by Pass.Check ___________________________________________

    val CheckEnv = new GuardedBy[Env](merge)
    def checkEnv = Env.empty(global) // CheckEnv.v
    
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

    // ___ Schedule unscheduled intervals ___________________________________
    //
    // It is important that this happen last.  This is because the header
    // interval may begin and try to read a field, such as SuperClassNames,
    // which has not yet been initialized!

    header.schedule()
    lower.schedule()
    
}