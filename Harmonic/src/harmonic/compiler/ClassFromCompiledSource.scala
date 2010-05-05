package harmonic.compiler

abstract class ClassFromCompiledSource(
    name: Name.Class,
    global: Global
) extends ClassSymbol(name, global) 
{
    def internalImplName = name.internalName
    def pos = InterPosition.forClassNamed(name)
    def optInterval(idx: Int) = None
    def setMethodGroups(groups: List[MethodGroup]) {}
}

