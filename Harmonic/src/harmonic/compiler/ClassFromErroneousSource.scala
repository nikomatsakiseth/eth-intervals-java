package harmonic.compiler

class ClassFromErroroneousSource(
    name: Name.Class,
    global: Global
) extends ClassFromCompiledSource(name, global) {
    def modifiers = Modifier.Set.empty
    def constructors = Nil
    def superClassNames = Nil
    def methodsNamed(name: Name.Method) = Nil
    def fieldNamed(name: Name.Member) = None
    def allMethodSymbols = Nil
    def varMembers = Nil
}

