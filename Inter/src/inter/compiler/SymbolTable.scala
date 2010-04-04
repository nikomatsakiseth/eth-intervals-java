package inter.compiler

/** Contains information about the class interfaces. */
class SymbolTable
{
    sealed abstract class Symbol
    
    class ClassSymbol(
        val name: Name.Qual
    ) extends Symbol {
    }
    
    class MethodSymbol(
        val name: Name.Method
    ) extends Symbol {
    }
    
}