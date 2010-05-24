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
        val header: AsyncInterval = master.subinterval(
            name = "%s.Header".format(name)
        ) { _ =>
            debug("Load data for %s", name)
            val rawData = loadData
            LoadedData.v = rawData.copy(
                superClassNames = ResolveHeader.cookRawSuperClasses(this, rawData.superClassNames)
            )
            debug("Loaded")
        }

        val body: AsyncInterval = master.subinterval(
            name = "%s.Body".format(name),
            after = List(header.getEnd)
        ) { 
            _ => ()
        }

        val lower: AsyncInterval = master.subinterval(
            name = "%s.Lower".format(name),
            after = List(body.getEnd),
            // Scheduled explicitly so we can add
            // create/members/merge within
            schedule = false 
        ) { 
            _ => ()
        }

        val create: AsyncInterval = master.subinterval(
            name = "%s.Create".format(name),
            during = List(lower)
        ) {
            _ => ()
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
            _ => ()
        }

        val gather: AsyncInterval = master.subinterval(
            name = "%s.Gather".format(name),
            after = List(lower.getEnd)
        ) {
            _ => GatherOverrides(global).forSym(this)
        }

        val byteCode: AsyncInterval = master.subinterval(
            name = "%s.ByteCode".format(name),
            after = List(gather.getEnd)
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
    
    def header: AsyncInterval = intervals("header")
    def body: AsyncInterval = intervals("body")
    def lower: AsyncInterval = intervals("lower")
    def create: AsyncInterval = intervals("create")
    def members: AsyncInterval = intervals("members")
    def merge: AsyncInterval = intervals("merge")
    def gather: AsyncInterval = intervals("gather")
    def byteCode: AsyncInterval = intervals("byteCode")
    
}

