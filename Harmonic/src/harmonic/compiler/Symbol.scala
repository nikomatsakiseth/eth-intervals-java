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
    
    sealed abstract class Class(
        val name: Name.Qual
    ) extends Ref {
        
        /** When a new instance of this class is created, what should we REALLY instantiate? */
        def internalImplName: String
        
        /** A position for reporting errors related to the class as a whole */
        def pos: Position
        
        /** List of all constructors for this class */
        def constructors(state: CompilationState): List[Symbol.Method]
        
        /** Names of any superclasses */
        def superClassNames(state: CompilationState): List[Name.Qual]
        
        /** Symbols for methods defined on this class (not superclasses) 
          * with the given name.  May trigger lowering or other processing. */
        def methodsNamed(state: CompilationState)(name: Name.Method): List[Symbol.Method]
        
        /** Symbols for fields defined on this class (not superclasses)
          * with the given name.  May trigger lowering or other processing. */
        def fieldNamed(state: CompilationState)(name: Name.Var): Option[Symbol.Var]
        
        /** Symbols for all methods defined on this class but not superclasses.
          * Should only be used after lowering has been completed. */
        def allMethodSymbols(state: CompilationState): List[Symbol.Method]
        
        /** Creates a `Type.Class` for the class defined by this symbol. */
        def toType: Type.Class = Type.Class(name, List())
        
        /** True if `this` is a subclass of `csym_sup` */
        def isSubclass(state: CompilationState, superCsym: Symbol.Class) = {
            (this == superCsym) || {
                val queued = new mutable.Queue[Symbol.Class]()
                val visited = new mutable.HashSet[Symbol.Class]()
                queued += this
                while(!queued.isEmpty && !visited(superCsym)) {
                    val nextCsym = queued.dequeue()
                    visited += nextCsym
                    queued ++= nextCsym.superClassNames(state).map(state.classes).filterNot(visited)
                }
                visited(superCsym)                
            }
        }

        override def toString = "%s(%s, %x)".format(
            getClass.getSimpleName, name, System.identityHashCode(this)
        )
              
    }
    
    class ClassFromErroroneousSource(
        name: Name.Qual
    ) extends Class(name) {
        def internalImplName = name.internalName
        def modifiers(state: CompilationState) = Modifier.Set.empty
        def constructors(state: CompilationState) = List()
        def superClassNames(state: CompilationState) = List()
        def methodsNamed(state: CompilationState)(name: Name.Method) = List()
        def fieldNamed(state: CompilationState)(name: Name.Var) = None
        def allMethodSymbols(state: CompilationState) = List()
        def pos = InterPosition.unknown
    }
    
    class ClassFromClassFile(
        name: Name.Qual,
        val file: java.io.File
    ) extends Class(name) {
        def internalImplName = name.internalName
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
            methods.filter(_.isNamed(name))
        }
        
        def allMethodSymbols(state: CompilationState) = {
            load(state)
            methods
        }
        
        def fieldNamed(state: CompilationState)(name: Name.Var) = {
            load(state)
            fields.find(_.isNamed(name))
        }
    }
    
    class ClassFromReflection(
        name: Name.Qual,
        val cls: java.lang.Class[_]
    ) extends Class(name) {
        def internalImplName = name.internalName
        var optCtors: Option[List[Symbol.Method]] = None
        var optMethods: Option[List[Symbol.Method]] = None
        var optFields: Option[List[Symbol.Var]] = None
        def pos = InterPosition.forClass(cls)
        
        def modifiers(state: CompilationState) = Modifier.forClass(cls)
        
        def constructors(state: CompilationState) = {
            Reflect(state).ctors(this)
        }
        def superClassNames(state: CompilationState) = List() // FIXME TODO
        def methodsNamed(state: CompilationState)(name: Name.Method) = {
            Reflect(state).methods(this).filter(_.isNamed(name))
        }
        def allMethodSymbols(state: CompilationState) = {
            Reflect(state).methods(this)
        }
        def fieldNamed(state: CompilationState)(name: Name.Var) = {
            Reflect(state).fields(this).find(_.isNamed(name))
        }
    }
    
    class ClassFromSource(
        name: Name.Qual
    ) extends Class(name) {
        def internalImplName = name.internalName + ByteCode.implSuffix
        
        def isNamed(aName: Name.Qual) = (name == aName)
        
        def pos = resolvedSource.pos
        
        /** Class declaration with names fully resolved. */
        var resolvedSource: Ast.Resolve.ClassDecl = null
        
        /** Class declaration in lowered form. */
        var loweredSource: Ast.Lower.ClassDecl = null
        
        /** Method symbol for constructor, once defined. */
        var optCtorSymbol: Option[Symbol.Method] = None
        
        /** Method symbols defined in this class, grouped by name. 
          * These symbols are created on demand but will be fully
          * instantiated by the end of the lowering phase. 
          * Generally preferred to use methodsNamed()() rather than
          * accessing this field directly. */
        val methodSymbols = new mutable.HashMap[Name.Method, List[Method]]()
        
        /** Cache of lowered version of each method. 
          * The cache is used to avoid lowering a method twice during
          * the lowering phase. */
        val loweredMethods = new mutable.HashMap[MethodId, Ast.Lower.MethodDecl]()
        
        /** A complete list of all versions of all methods offered by this class,
          * whether they are defined in this class or in a superclass. Populated 
          * by GatherOverrides for all classes being compiled and their supertypes. */
        var methodGroups: List[MethodGroup] = Nil
        
        def modifiers(state: CompilationState) = 
            Modifier.forResolvedAnnotations(resolvedSource.annotations)
        
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
        
        def allMethodSymbols(state: CompilationState) = 
            // Because this method can only be invoked after lowering,
            // methodSymbols hash is fully populated:
            methodSymbols.valuesIterator.toList.flatten
        
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
        
        def isFromClassNamed(aName: Name.Qual) = (clsName == aName)
        def isNamed(aName: Name.Method) = (name == aName)
        
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
        def isNamed(aName: Name.Var) = (name == aName)
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
    
    /** A method group consists of a list of method symbols which override one
      * another.  Methods override one another when (a) they have the same name
      * and the same static parameter types and (b) they are both inherited by
      * a single class. 
      * 
      * Note that two methods defined in unrelated classes A and B
      * can still override one another if a third class C extends both A and B
      * (though this scenario generally results in a static error unless C redefines
      * the method so as to clarify which version, A's or B's, it prefers). */
    class MethodGroup( 
        /** Method name */
        val methodName: Name.Method,
        
        /** A representative method signature */
        val msig: MethodSignature[Pattern.Ref]
    ) {
        /** List of method symbols implementing this method, in MRO order. 
          * Note that some classes may define the same method more than once
          * with various signatures.  In such cases, all of those versions are
          * included in this list, but the first one contains the implementation
          * which should actually be invoked (if any). */
        private[this] val msymsBuffer = new mutable.ListBuffer[Symbol.Method]()
        
        def addMsym(msym: Symbol.Method) = msymsBuffer += msym
        def msyms = msymsBuffer.toList
    }
    
}