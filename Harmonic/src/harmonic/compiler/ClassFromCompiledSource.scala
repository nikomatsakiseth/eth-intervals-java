package harmonic.compiler

import ch.ethz.intervals._
import Util._

abstract class ClassFromCompiledSource extends ClassSymbol
{
    implicit val implicitGlobal = global
    import global.master
    import global.debugServer
    
    private[this] val classPage = debugServer.pageForClass(name)
    
    def internalImplName = name.internalName
    
    def pos = InterPosition.forClassNamed(name)
    
    def setMethodGroups(groups: List[MethodGroup]) {}

    // ___ Class Data _______________________________________________________
    //
    // What all ClassFromCompiledSource instances have in common is that they
    // are lazilly loaded.  Doing this lazilly prevents us from loading the
    // full transitive closure unless it is needed.  The load method will be
    // invoked at most once from the `header` interval.
    //
    // Note: The loadData method must invoke requireLoadedOrLoadable on all
    // class names.
    
    case class Data(
        modifiers: Modifier.Set,
        superClassNames: List[Name.Class],
        varMembers: List[SymTab.Entry],
        constructors: List[MethodSymbol],
        allMethodSymbols: List[MethodSymbol],
        allFieldSymbols: List[VarSymbol.Field],
        allIntervalSymbols: List[VarSymbol.Field],
        checkEnv: Env
    )
    
    protected[this] def loadData(current: Interval): Data
    
    lazy val LoadedData = new GuardedBy[Data](header)
    final def modifiers: Modifier.Set = LoadedData.join.modifiers
    final def superClassNames: List[Name.Class] = LoadedData.join.superClassNames
    final def varMembers: List[SymTab.Entry] = LoadedData.join.varMembers
    final def constructors: List[MethodSymbol] = LoadedData.join.constructors
    final def allMethodSymbols: List[MethodSymbol] = LoadedData.join.allMethodSymbols
    final def allFieldSymbols: List[VarSymbol.Field] = LoadedData.join.allFieldSymbols
    final def allIntervalSymbols: List[VarSymbol.Field] = LoadedData.join.allIntervalSymbols
    final def methodsNamed(name: Name.Method): List[MethodSymbol] = allMethodSymbols.filter(_.isNamed(name))
    final def fieldNamed(name: Name.Member): Option[VarSymbol.Field] = allFieldSymbols.find(_.isNamed(name))
    final def checkEnv = LoadedData.join.checkEnv
    
    lazy val Mro = new GuardedBy[List[ClassSymbol]](cmro)
    final def mro: List[ClassSymbol] = Mro.join
    
    // ___ Interval Creation ________________________________________________
    //
    // Most of these intervals do not have to do anything as this class has
    // already been compiled.  The `header` interval is where we load the
    // class data, and create any necessary edges between our intervals and
    // those of our superclass.  The `gather` interval still must compute
    // the overrides for all of our method symbols.
    
    def header: AsyncInterval = intervals("header")
    def cmro: AsyncInterval = intervals("cmro")
    def body: AsyncInterval = intervals("body")
    def lower: AsyncInterval = intervals("lower")
    def create: AsyncInterval = intervals("create")
    def members: AsyncInterval = intervals("members")
    def merge: AsyncInterval = intervals("merge")
    def envirate: AsyncInterval = intervals("envirate")
    def check: AsyncInterval = intervals("check")
    def gather: AsyncInterval = intervals("gather")
    def byteCode: AsyncInterval = intervals("byteCode")
    
    private[this] lazy val intervals = {
        val header: AsyncInterval = master.subinterval(
            schedule = false,
            name = "%s.Header".format(name),
            parentPage = classPage
        ) { 
            inter => {
                val rawData = loadData(inter)
                LoadedData.v = rawData.copy(
                    superClassNames = ResolveHeader.cookRawSuperClasses(this, rawData.superClassNames)
                )
            }
        }
        
        val cmro: AsyncInterval = master.subinterval(
            name = "%s.cmro".format(name),
            parentPage = classPage,
            after = List(header.getEnd)
        ) {
            inter => {
                Mro.v = MethodResolutionOrder(global).computeForSym(this)
            }
        }

        val body: AsyncInterval = master.subinterval(
            name = "%s.Body".format(name),
            parentPage = classPage,
            after = List(header.getEnd)
        ) { 
            inter => {
            }
        }

        val lower: AsyncInterval = master.subinterval(
            name = "%s.Lower".format(name),
            parentPage = classPage,
            after = List(body.getEnd, cmro.getEnd),
            // Scheduled explicitly so we can add
            // create/members/merge within
            schedule = false 
        ) { 
            _ => {
            }
        }

        val create: AsyncInterval = master.subinterval(
            name = "%s.Create".format(name),
            parentPage = classPage,
            during = List(lower)
        ) {
            inter => {
            }
        }

        val members: AsyncInterval = master.subinterval(
            name = "%s.Members".format(name),
            parentPage = classPage,
            during = List(lower), 
            after = List(create.getEnd)
        ) { 
            inter => {
            }
        }

        val merge: AsyncInterval = master.subinterval(
            name = "%s.Merge".format(name),
            parentPage = classPage,
            during = List(lower), 
            after = List(members.getEnd)
        ) { 
            inter => {
            }
        }
        
        val envirate: AsyncInterval = master.subinterval(
            name = "%s.Envirate".format(name),
            parentPage = classPage,
            after = List(lower.getEnd)
        ) {
            inter => {
            }
        }
        
        val check: AsyncInterval = master.subinterval(
            name = "%s.Check".format(name),
            parentPage = classPage,
            after = List(envirate.getEnd)
        ) {
            inter => {
            }
        }

        val gather: AsyncInterval = master.subinterval(
            name = "%s.Gather".format(name),
            parentPage = classPage,
            after = List(envirate.getEnd)
        ) {
            inter => {
                GatherOverrides(global).forSym(this)
            }
        }

        val byteCode: AsyncInterval = master.subinterval(
            name = "%s.ByteCode".format(name),
            parentPage = classPage,
            after = List(check.getEnd, gather.getEnd)
        ) { 
            inter => {
            }
        }
        
        header.schedule()
        lower.schedule()
        
        Map(
            "header" -> header,
            "cmro" -> cmro,
            "body" -> body,
            "lower" -> lower,
            "create" -> create,
            "members" -> members,
            "merge" -> merge,
            "envirate" -> envirate,
            "check" -> check,
            "gather" -> gather,
            "byteCode" -> byteCode
        )
    }
    
}

