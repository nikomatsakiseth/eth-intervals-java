package harmonic.compiler

import ch.ethz.intervals._
import Util._

abstract class ClassFromCompiledSource extends ClassSymbol
{
    import global.master
    
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
        allIntervalSymbols: List[VarSymbol.Field]
    )
    
    protected[this] def loadData: Data
    
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
    
    // ___ Interval Creation ________________________________________________
    //
    // Most of these intervals do not have to do anything as this class has
    // already been compiled.  The `header` interval is where we load the
    // class data, and create any necessary edges between our intervals and
    // those of our superclass.  The `gather` interval still must compute
    // the overrides for all of our method symbols.
    
    private[this] lazy val intervals = {
        val header: Interval = master.subinterval(
            name = "%s.Header".format(name)
        ) { _ =>
            debug("Load data for %s", name)
            val rawData = loadData
            LoadedData.v = rawData.copy(
                superClassNames = ResolveHeader.cookRawSuperClasses(this, rawData.superClassNames)
            )
            debug("Loaded")
        }

        val body: Interval = master.subinterval(
            name = "%s.Body".format(name),
            after = List(header.end)
        ) { 
            _ => ()
        }

        val lower: Interval = master.subinterval(
            name = "%s.Lower".format(name),
            after = List(body.end),
            // Scheduled explicitly so we can add
            // create/members/merge within
            schedule = false 
        ) { 
            _ => ()
        }

        val create: Interval = master.subinterval(
            name = "%s.Create".format(name),
            during = List(lower)
        ) {
            _ => ()
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
            _ => ()
        }

        val gather: Interval = master.subinterval(
            name = "%s.Gather".format(name),
            after = List(lower.end)
        ) {
            _ => GatherOverrides(global).forSym(this)
        }

        val byteCode: Interval = master.subinterval(
            name = "%s.ByteCode".format(name),
            after = List(gather.end)
        ) { 
            _ => ()
        }
        
        lower.schedule()
        
        Map(
            "header" -> header,
            "body" -> body,
            "lower" -> lower,
            "create" -> create,
            "members" -> members,
            "merge" -> merge,
            "gather" -> gather,
            "byteCode" -> byteCode
        )
    }
    
    def header: Interval = intervals("header")
    def body: Interval = intervals("body")
    def lower: Interval = intervals("lower")
    def create: Interval = intervals("create")
    def members: Interval = intervals("members")
    def merge: Interval = intervals("merge")
    def gather: Interval = intervals("gather")
    def byteCode: Interval = intervals("byteCode")
    
}

