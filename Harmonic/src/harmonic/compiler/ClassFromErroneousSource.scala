package harmonic.compiler

class ClassFromErroroneousSource(
    name: Name.Class
) extends ClassFromCompiledSource(name) {
    def modifiers = Modifier.Set.empty
    def constructors = Nil
    def superClassNames = Nil
    def methodsNamed(name: Name.Method) = Nil
    def fieldNamed(name: Name.Member) = None
    def allMethodSymbols = Nil
    def varMembers = Nil
}

