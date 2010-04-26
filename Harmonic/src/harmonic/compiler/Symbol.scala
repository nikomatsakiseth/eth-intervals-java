package harmonic.compiler

import scala.collection.mutable
import scala.util.parsing.input.Position

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
        def modifiers(state: CompilationState): Modifier.Set
        def isError: Boolean = false
    }
    
    abstract class Class(
        val name: Name.Qual
    ) extends Ref {
        def constructors(state: CompilationState): List[Symbol.Method]
        def superClassNames(state: CompilationState): List[Name.Qual]
        def methodsNamed(state: CompilationState)(name: Name.Method): List[Symbol.Method]
        def fieldNamed(state: CompilationState)(name: Name.Var): Option[Symbol.Var]
        def pos: Position
        
        override def toString = "%s(%s, %x)".format(
            getClass.getSimpleName, name, System.identityHashCode(this)
        )        
    }
    
    class ClassFromErroroneousSource(
        name: Name.Qual
    ) extends Class(name) {
        def modifiers(state: CompilationState) = Modifier.Set.empty
        def constructors(state: CompilationState) = List()
        def superClassNames(state: CompilationState) = List()
        def methodsNamed(state: CompilationState)(name: Name.Method) = List()
        def fieldNamed(state: CompilationState)(name: Name.Var) = None
        def pos = InterPosition.unknown
    }
    
    class ClassFromClassFile(
        name: Name.Qual,
        val file: java.io.File
    ) extends Class(name) {
        var modifierSet = Modifier.Set.empty
        var loaded = false
        var constructors = List[Symbol.Method]()
        var superClassNames = List[Name.Qual]()
        var methods = List[Symbol.Method]()
        var fields = List[Symbol.Var]()
        def pos = InterPosition.forFile(file)
        
        def load(state: CompilationState) {
            if(!loaded) {
                LoadClassFile(state, this)
                loaded = true
            }
        }
        
        def modifiers(state: CompilationState) = {
            load(state)
            modifierSet
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
        var optCtors: Option[List[Symbol.Method]] = None
        var optMethods: Option[List[Symbol.Method]] = None
        var optFields: Option[List[Symbol.Var]] = None
        def pos = InterPosition.forClass(cls)
        
        def modifiers(state: CompilationState) = Modifier.forClass(cls)
        
        def constructors(state: CompilationState) = {
            Reflect(state).ctors(this)
        }
        def superClassNames(state: CompilationState) = List() // XXX TODO
        def methodsNamed(state: CompilationState)(name: Name.Method) = {
            Reflect(state).methodsNamed(this, name)
        }
        def fieldNamed(state: CompilationState)(name: Name.Var) = {
            Reflect(state).fieldNamed(this, name)
        }
    }
    
    class ClassFromInterFile(
        name: Name.Qual
    ) extends Class(name) {
        var resolvedSource: Ast.Resolve.ClassDecl = null
        var loweredSource: Ast.Lower.ClassDecl = null
        var optCtorSymbol: Option[Symbol.Method] = None
        val methodSymbols = new mutable.HashMap[Name.Method, List[Method]]()
        val loweredMethods = new mutable.HashMap[MethodId, Ast.Lower.MethodDecl]()
        def pos = resolvedSource.pos
        
        def allMethodSymbols = methodSymbols.valuesIterator.toList.flatten
        
        def modifiers(state: CompilationState) = Modifier.forResolvedAnnotations(resolvedSource.annotations)
        
        def constructors(state: CompilationState) = {
            optCtorSymbol match {
                case Some(ctorSymbol) => List(ctorSymbol)
                case None => {
                    val ctorSymbol = new Symbol.Method(
                        modifierSet = modifiers(state),
                        kind = Symbol.InterCtor,
                        clsName = name,
                        name = Name.InitMethod,
                        Symbol.MethodSignature(
                            returnTy = Type.Void,
                            receiverTy = Type.Class(name, List()),
                            parameterPatterns = List(Lower(state).symbolPattern(resolvedSource.pattern))
                        )
                    )
                    optCtorSymbol = Some(ctorSymbol)
                    List(ctorSymbol)
                }
            }
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
    case class IntrinsicMath(
        staticMthdName: String,
        leftClass: java.lang.Class[_], 
        rightClass: java.lang.Class[_], 
        resultClass: java.lang.Class[_]
    ) extends MethodKind
    case class IntrinsicStatic(
        ownerClass: java.lang.Class[_],
        staticMthdName: String,
        argumentClasses: Array[java.lang.Class[_]],
        resultClass: java.lang.Class[_]
    ) extends MethodKind
    case object Inter extends MethodKind
    case object InterCtor extends MethodKind
    case object JavaVirtual extends MethodKind
    case object JavaInterface extends MethodKind
    case object JavaStatic extends MethodKind
    case object ErrorMethod extends MethodKind
    
    class Method(
        val modifierSet: Modifier.Set,
        val kind: MethodKind,                   /** Intrinsic, harmonic, java, etc. */
        val clsName: Name.Qual,                 /** Class in which the method is defined. */
        val name: Name.Method,                  /** Name of the method. */
        val msig: MethodSignature[Pattern.Ref]
    ) extends Ref {
        override def toString = "Method(%s, %x)".format(name, System.identityHashCode(this))
        
        def methodId = MethodId(clsName, name, msig.parameterPatterns)
        
        def modifiers(state: CompilationState) = modifierSet
        
        /** For methods whose source will be emitted, we compute the 
          * overridden methods.  The ordering is significant,
          * because when super is invoked it will proceed to the
          * next implementation in the list.  For methods on
          * reflected classes or classes loaded from .class files,
          * this list is simply empty. */
        val overrides = new mutable.ListBuffer[Symbol.Method]()
    }
    
    def errorMethod(name: Name.Method, clsName: Name.Qual) = {
        val parameterPatterns = name.parts.zipWithIndex.map { case (_, i) => 
            Pattern.Var(Name.Var("arg%d".format(i)), Type.Null)
        }
        new Method(
            Modifier.Set.empty,
            ErrorMethod, 
            clsName, 
            name, 
            MethodSignature(Type.Null, Type.Null, parameterPatterns)
        ) {
            override def isError = true
        }
    }
    
    case class MethodSignature[+P <: Pattern.Anon](
        val returnTy: Type.Ref,
        val receiverTy: Type.Ref,
        val parameterPatterns: List[P]        
    ) {
        def receiverAnonPattern: Pattern.AnonVar = Pattern.SubstdVar(receiverTy)
        def thisPattern: Pattern.Var = Pattern.Var(Name.ThisVar, receiverTy)
    }
    
    class Var(
        val modifierSet: Modifier.Set,
        val name: Name.Var,
        val ty: Type.Ref
    ) extends Ref {
        override def toString = "Var(%s, %x)".format(name, System.identityHashCode(this))
        def modifiers(state: CompilationState) = modifierSet
        
    }
    
    def errorVar(name: Name.Var, optExpTy: Option[Type.Ref]) = {
        val ty = optExpTy.getOrElse(Type.Null)
        new Var(Modifier.Set.empty, name, ty) {
            override def isError = true
        }
    }
    
    abstract class MemberId
    case class MethodId(clsName: Name.Qual, methodName: Name.Method, parameterPatterns: List[Pattern.Ref]) extends MemberId
    case class FieldId(clsName: Name.Qual, methodName: Name.Method) extends MemberId
    
    def superclasses(state: CompilationState, csym: Symbol.Class) = {
        val queued = new mutable.Queue[Symbol.Class]()
        val visited = new mutable.HashSet[Symbol.Class]()
        queued += csym
        while(!queued.isEmpty) {
            val csym_next = queued.dequeue()
            visited += csym_next
            queued ++= csym_next.superClassNames(state).map(state.classes).filterNot(visited)
        }
        visited
    }
    
    def isSubclass(state: CompilationState, csym_sub: Symbol.Class, csym_sup: Symbol.Class) = {
        (csym_sub == csym_sup) || Symbol.superclasses(state, csym_sub).contains(csym_sup)
    }
    
}