package inter.compiler

import scala.collection.mutable.ListBuffer

/** Contains information about the class interfaces. 
  * Built up in a mutable fashion. */
class SymbolTable
{
    
    sealed abstract class Symbol
    
    class ClassSymbol(
        val name: Name.Qual
    ) extends Symbol {
        val methods = new ListBuffer[MethodSymbol]()
        val fields = new ListBuffer[VarSymbol]()
    }
    
    class MethodSymbol(
        val name: Name.Method,
        val retTypeRef: Type,
        val parameters: List[VarSymbol]
    ) extends Symbol
    
    class VarSymbol(
        val name: Name.Var,
        val typeRef: Type
    ) extends Symbol
    
    sealed abstract class Type
    case class PathType(path: Name.Path, typeVar: Name.Var) extends Type
    case class ClassType(name: Name.Qual, typeArgs: List[TypeArg]) extends Type
    
    sealed abstract class TypeArg
    case class PathTypeArg(name: Name.Var, rel: PcRel, path: Name.Path) extends TypeArg
    case class TypeTypeArg(name: Name.Var, rel: TcRel, typeRef: Type) extends TypeArg
    
}