package inter.compiler

/** Contains information about the class interfaces. 
  * Built up in a mutable fashion.
  *
  * 
  */
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
    
    class ParameterSymbol(
        val name: Name.Var
    ) extends Symbol {
    }
 
    class FieldSymbol(
        val name: Name.Var
    ) extends Symbol {
    }
    
    type Path = List[String]
    
    sealed abstract class Type
    case class PathType(path: Path, typeVar: Name.Var) extends Type
    case class ClassType(name: Name.Qual, typeArgs: List[TypeArg]) extends Type
    
    sealed abstract class TypeArg
    case class PathTypeArg(name: Name.Var, rel: PcRel, path: Path) extends TypeArg
    case class TypeTypeArg(name: Name.Var, rel: TcRel, typeRef: Type) extends TypeArg
    
}