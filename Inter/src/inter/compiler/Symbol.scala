package inter.compiler

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

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
    
    abstract class Class(
        val name: Name.Qual
    ) {
        def constructors(state: CompilationState): Seq[Type]
        def superClassNames(state: CompilationState): Seq[Name.Qual]
        def methodsNamed(state: CompilationState)(name: Name.Method): List[Symbol.Method]
        def fieldNamed(state: CompilationState)(name: Name.Var): Option[Symbol.Var]
    }
    
    class ClassFromErroroneousSource(
        name: Name.Qual
    ) extends Class(name) {
        def constructors(state: CompilationState) = List()
        def superClassNames(state: CompilationState) = List()
        def methodsNamed(state: CompilationState)(name: Name.Method) = List()
        def fieldNamed(state: CompilationState)(name: Name.Var) = None
    }
    
    class ClassFromClassFile(
        name: Name.Qual,
        val file: java.io.File
    ) extends Class(name) {
        var loaded = false
        var constructors = List[Type]()
        var superClassNames = List[Name.Qual]()
        var methods = List[Symbol.Method]()
        var fields = List[Symbol.Var]()
        
        def load(state: CompilationState) {
            if(!loaded) {
                LoadClassFile(state, this)
                loaded = true
            }
        }
        
        def constructors(state: CompilationState) = {
            load(state)
            constructors
        }
        
        def superClassNames(state: CompilationState) = {
            load(state)
            superClassNames
        }
        
        def methodsNamed(state: CompilationState)(name: Name.Method) = {
            load(state)
            methods.filter(_.name == name)
        }
        
        def fieldNamed(state: CompilationState)(name: Name.Var) = {
            load(state)
            fields.find(_.name == name)
        }
    }
    
    class ClassFromReflection(
        name: Name.Qual,
        val cls: java.lang.Class[_]
    ) extends Class(name) {
        var optMethods: Option[List[Symbol.Method]] = None
        var optFields: Option[List[Symbol.Var]] = None
        
        def constructors(state: CompilationState) = List() // ΧΧΧ TODO
        def superClassNames(state: CompilationState) = List() // XXX TODO
        def methodsNamed(state: CompilationState)(name: Name.Method) = {
            Reflect.methodsNamed(state, this, name)
        }
        def fieldNamed(state: CompilationState)(name: Name.Var) = {
            Reflect.fieldNamed(state, this, name)
        }
    }
    
    class ClassFromInterFile(
        name: Name.Qual
    ) extends Class(name) {
        var resolvedSource: Ast.Resolve.ClassDecl = null
        val methods = new HashMap[Name.Method, List[Method]]()
        
        def constructors(state: CompilationState) = {
            List(Lower.patternType(resolvedSource.pattern))
        }
        
        def superClassNames(state: CompilationState) = {
            resolvedSource.superClasses.map(_.qualName)            
        }
        
        def methodsNamed(state: CompilationState)(memName: Name.Method) = {
            Lower.symbolsForMethodsNamed(state, this, memName)
        }
        
        def fieldNamed(state: CompilationState)(name: Name.Var) = None
    }
    
    class Method(
        val name: Name.Method,
        val returnTy: Symbol.Type,
        val receiver: Symbol.VarPattern,
        val parameterPatterns: List[Symbol.Pattern]
    )
    
    object ErrorMethod extends Method(
        Name.Method(List("<error>")), NullType, VarPattern(Name.ThisVar, NullType), List()
    )
    
    class Var(
        val name: Name.Var,
        val ty: Type
    ) extends Pattern
    
    def errorVar(name: Name.Var, optExpTy: Option[Type]) = {
        optExpTy match {
            case None => new Var(name, NullType)
            case Some(ty) => new Var(name, ty)
        }
    }
    
    sealed abstract class Pattern {
        def ty: Type
    }
    sealed case class VarPattern(
        val name: Name.Var,
        val ty: Type
    ) extends Pattern 
    sealed case class TuplePattern(patterns: List[Pattern]) extends Pattern {
        def ty = TupleType(patterns.map(_.ty))
    }
    
    sealed abstract class Type
    case class PathType(path: Name.Path, typeVar: Name.Var) extends Type
    case class ClassType(name: Name.Qual, typeArgs: List[TypeArg]) extends Type
    case class TupleType(typeRefs: List[Type]) extends Type
    case object NullType extends Type
    
    sealed abstract class TypeArg
    case class PathTypeArg(name: Name.Var, rel: PcRel, path: Name.Path) extends TypeArg
    case class TypeTypeArg(name: Name.Var, rel: TcRel, typeRef: Type) extends TypeArg
    
    def createVarSymbols(p: Pattern): List[Var] = p match {
        case VarPattern(name, ty) => List(new Var(name, ty))
        case TuplePattern(patterns) => patterns.flatMap(createVarSymbols)
    }
    
    val VoidType = ClassType(Name.VoidQual, List())
        
}