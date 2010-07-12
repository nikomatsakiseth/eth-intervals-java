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
    
    lazy val AllOverrides = new GuardedBy[Map[MethodSymbol, List[MethodSymbol]]](gather)
    
    // ___ Interval Creation ________________________________________________
    //
    // Most of these intervals do not have to do anything as this class has
    // already been compiled.  The `header` interval is where we load the
    // class data, and create any necessary edges between our intervals and
    // those of our superclass.  For classes which are extended by classes
    // that are currently being compiled, the `gather` interval still must compute
    // the overrides for all of our method symbols.
    //
    // Note that all interval creation is lazy: only when we see that data is
    // needed do we do any work, and even if the `header` interval is created,
    // other intervals like the `gather` interval may not be.
    
    lazy val header: AsyncInterval = master.subinterval(
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
    
    lazy val cmro: AsyncInterval = master.subinterval(
        name = "%s.cmro".format(name),
        parentPage = classPage,
        after = List(header.getEnd)
    ) {
        inter => {
            Mro.v = MethodResolutionOrder(global).computeForSym(this)
        }
    }

    lazy val body: AsyncInterval = master.subinterval(
        name = "%s.Body".format(name),
        parentPage = classPage,
        after = List(header.getEnd)
    ) { 
        inter => {
        }
    }

    lazy val lower: AsyncInterval = master.subinterval(
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

    lazy val create: AsyncInterval = master.subinterval(
        name = "%s.Create".format(name),
        parentPage = classPage,
        during = List(lower)
    ) {
        inter => {
        }
    }

    lazy val members: AsyncInterval = master.subinterval(
        name = "%s.Members".format(name),
        parentPage = classPage,
        during = List(lower), 
        after = List(create.getEnd)
    ) { 
        inter => {
        }
    }

    lazy val merge: AsyncInterval = master.subinterval(
        name = "%s.Merge".format(name),
        parentPage = classPage,
        during = List(lower), 
        after = List(members.getEnd)
    ) { 
        inter => {
        }
    }
    
    lazy val envirate: AsyncInterval = master.subinterval(
        name = "%s.Envirate".format(name),
        parentPage = classPage,
        after = List(lower.getEnd)
    ) {
        inter => {
        }
    }
    
    lazy val check: AsyncInterval = master.subinterval(
        name = "%s.Check".format(name),
        parentPage = classPage,
        after = List(envirate.getEnd)
    ) {
        inter => {
        }
    }

    lazy val gather: AsyncInterval = master.subinterval(
        name = "%s.Gather".format(name),
        parentPage = classPage,
        after = List(envirate.getEnd)
    ) {
        inter => {
            GatherOverrides(global).forSym(this)
        }
    }

    lazy val byteCode: AsyncInterval = master.subinterval(
        name = "%s.ByteCode".format(name),
        parentPage = classPage,
        after = List(check.getEnd, gather.getEnd)
    ) { 
        inter => {
        }
    }
    
}

