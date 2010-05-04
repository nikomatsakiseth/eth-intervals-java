package harmonic.compiler

class ClassFromClassFile(
    name: Name.Class,
    global: Global,
    file: java.io.File
) extends ClassFromCompiledSource(name, global) {
    private[this] var loaded = false
    private[this] var vModifiers = Modifier.Set.empty
    private[this] var vConstructors = List[MethodSymbol]()
    private[this] var vSuperClassNames = List[Name.Qual]()
    private[this] var vMethods = List[MethodSymbol]()
    private[this] var vFields = List[VarSymbol.Field]()
    private[this] var vVarMembers = List[SymTab.Entry]()
    
    override def pos = InterPosition.forFile(file)
    
    def modifiers = {
        load(state)
        vModifiers
    }
    
    def constructors = {
        load(state)
        vConstructors
    }
    
    def superClassNames = {
        load(state)
        vSuperClassNames
    }
    
    def varMembers = {
        load(state)
        vVarMembers
    }
    
    def methodsNamed(name: Name.Method) = {
        load(state)
        vMethods.filter(_.isNamed(name))
    }
    
    def allMethodSymbols = {
        load(state)
        vMethods
    }
    
    def fieldNamed(name: Name.Member) = {
        load(state)
        vFields.find(_.isNamed(name))
    }
    
    // ___ Class File Loading _______________________________________________
    
    def load(state: State) = synchronized {
        if(!loaded) {
            throw new RuntimeException("TODO-- Implement .class file loading!")
            loaded = true
        }
    }
    
}

