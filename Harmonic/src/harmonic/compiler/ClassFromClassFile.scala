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
        load()
        vModifiers
    }
    
    def constructors = {
        load()
        vConstructors
    }
    
    def superClassNames = {
        load()
        vSuperClassNames
    }
    
    def varMembers = {
        load()
        vVarMembers
    }
    
    def methodsNamed(name: Name.Method) = {
        load()
        vMethods.filter(_.isNamed(name))
    }
    
    def allMethodSymbols = {
        load()
        vMethods
    }
    
    def fieldNamed(name: Name.Member) = {
        load()
        vFields.find(_.isNamed(name))
    }
    
    // ___ Class File Loading _______________________________________________
    
    def load() = synchronized {
        if(!loaded) {
            throw new RuntimeException("TODO-- Implement .class file loading!")
            loaded = true
        }
    }
    
}

