package inter.compiler

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.Queue

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
    
    // ___ Types ____________________________________________________________    
    
    abstract class Ref {
        def isError: Boolean = false
    }
    
    abstract class Class(
        val name: Name.Qual
    ) extends Ref {
        def constructors(state: CompilationState): Seq[Type.Ref]
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
        var constructors = List[Type.Ref]()
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
        var loweredSource: Ast.Lower.ClassDecl = null
        val methodSymbols = new HashMap[Name.Method, List[Method]]()
        val loweredMethods = new HashMap[MethodId, Ast.Lower.MethodDecl]()
        
        def constructors(state: CompilationState) = {
            List(Lower(state).patternType(resolvedSource.pattern))
        }
        
        def superClassNames(state: CompilationState) = {
            resolvedSource.superClasses.map(_.qualName)            
        }
        
        def methodsNamed(state: CompilationState)(memName: Name.Method) = {
            Lower(state).symbolsForMethodsNamed(this, memName)
        }
        
        def fieldNamed(state: CompilationState)(name: Name.Var) = None
    }
    
    sealed abstract class MethodKind
    case object IntrinsicMath extends MethodKind
    case object Inter extends MethodKind
    case object JavaVirtual extends MethodKind
    case object JavaInterface extends MethodKind
    case object JavaStatic extends MethodKind
    case object ErrorMethod extends MethodKind
    
    class Method(
        val kind: MethodKind,
        val name: Name.Method,
        val returnTy: Type.Ref,
        val receiver: Pattern.Var,
        val parameterPatterns: List[Pattern.Ref]
    ) extends Ref
    
    def errorMethod(name: Name.Method) = {
        val receiver = Pattern.Var(Name.This, Type.Null)
        val parameterPatterns = name.parts.zipWithIndex.map { case (_, i) => 
            Pattern.Var(Name.Var("arg%d".format(i)), Type.Null)
        }
        new Method(ErrorMethod, name, Type.Null, receiver, parameterPatterns) {
            override def isError = true
        }
    }
    
    class Var(
        val name: Name.Var,
        val ty: Type.Ref
    ) extends Ref
    
    def errorVar(name: Name.Var, optExpTy: Option[Type.Ref]) = {
        val ty = optExpTy.getOrElse(Type.Null)
        new Var(name, ty) {
            override def isError = true
        }
    }
    
    abstract class MemberId
    case class MethodId(clsName: Name.Qual, methodName: Name.Method, parameterPatterns: List[Pattern.Ref]) extends MemberId
    case class FieldId(clsName: Name.Qual, methodName: Name.Method) extends MemberId
    
}