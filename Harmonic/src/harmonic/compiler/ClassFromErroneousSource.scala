package harmonic.compiler

class ClassFromErroroneousSource(
    val name: Name.Class,
    val global: Global
) extends ClassFromCompiledSource {
    protected[this] def loadData = Data(
        modifiers = Modifier.Set.empty,
        superClassNames = List(Name.ObjectClass),
        constructors = Nil,
        varMembers = Nil,
        allMethodSymbols = Nil,
        allFieldSymbols = Nil,
        allIntervalSymbols = Nil,
        checkEnv = Env.empty(global)
    )
}

