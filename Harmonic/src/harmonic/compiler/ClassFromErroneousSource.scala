package harmonic.compiler

import ch.ethz.intervals._

class ClassFromErroroneousSource(
    val name: Name.Class,
    val global: Global
) extends ClassFromCompiledSource {
    protected[this] def loadData(inter: Interval) = Data(
        modifiers = Modifier.Set.empty,
        superClassNames = List(Name.ObjectClass),
        superTypes = List(Type.Object),
        constructors = Nil,
        varMembers = Nil,
        allMethodSymbols = Nil,
        allFieldSymbols = Nil,
        allIntervalSymbols = Nil,
        allGhostSymbols = Nil,
        checkEnv = Env.empty(global)
    )
}

