package inter.compiler

import scala.collection.mutable.ListBuffer

/** Symbols describe the class interfaces.  Unlike the AST,
  * they are mutable data structures, built-up during the
  * various phases.  They can also describe classes built in
  * languages other than Inter (Java, for example). 
  *
  * Note that Symbols are not case classes.  This is because
  * they have identity independent of their name. That is,
  * two different method instances, for example, that have
  * the same fields may still represent distinct methods
  * (i.e., if they are implemented on different classes). */
object Symbol {
    
    class Class(
        val name: Name.Qual
    ) {
        val superClassNames = new ListBuffer[Name.Qual]()
        val methods = new ListBuffer[Symbol.Method]()
        val fields = new ListBuffer[Symbol.Var]()
    }
    
    class Method(
        val name: Name.Method,
        val retTypeRef: Type,
        val parameters: List[Symbol.Var]
    )
    
    class Var(
        val name: Name.Var,
        val typeRef: Type
    )
    
    sealed abstract class Type
    case class PathType(path: Name.Path, typeVar: Name.Var) extends Type
    case class ClassType(name: Name.Qual, typeArgs: List[TypeArg]) extends Type
    case class TupleType(typeRefs: List[Type]) extends Type
    
    sealed abstract class TypeArg
    case class PathTypeArg(name: Name.Var, rel: PcRel, path: Name.Path) extends TypeArg
    case class TypeTypeArg(name: Name.Var, rel: TcRel, typeRef: Type) extends TypeArg
    
}