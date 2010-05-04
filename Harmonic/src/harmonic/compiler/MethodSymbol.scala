package harmonic.compiler

import scala.util.parsing.input.Position

object MethodSymbol {
    
    // Just provides useful defaults.
    sealed abstract class Kind
    case class IntrinsicMath(
        staticMthdName: String,
        leftClass: java.lang.Class[_], 
        rightClass: java.lang.Class[_], 
        resultClass: java.lang.Class[_]
    ) extends Kind
    case class IntrinsicStatic(
        ownerClass: java.lang.Class[_],
        staticMthdName: String,
        argumentClasses: Array[java.lang.Class[_]],
        resultClass: java.lang.Class[_]
    ) extends Kind
    case object Inter extends Kind
    case object InterCtor extends Kind
    case object JavaVirtual extends Kind
    case object JavaInterface extends Kind
    case object JavaStatic extends Kind
    case object ErrorMethod extends Kind
    
    def error(name: Name.Method, clsName: Name.Class) = {
        val parameterPatterns = name.parts.zipWithIndex.map { case (_, i) => 
            Pattern.Var(Name.LocalVar("arg%d".format(i)), Type.Null)
        }
        new Method(
            pos         = InterPosition.forClassNamed(clsName),
            modifiers = Modifier.Set.empty,
            kind        = ErrorMethod, 
            clsName     = clsName, 
            name        = name, 
            MethodSignature(Type.Null, Type.Null, parameterPatterns)
        ) {
            override def isError = true
        }
    }
    
}

class MethodSymbol(
    val pos: Position,
    val modifiers: Modifier.Set,
    val kind: MethodSymbol.Kind,            /** Intrinsic, harmonic, java, etc. */
    val clsName: Name.Class,                /** Class in which the method is defined. */
    val name: Name.Method,                  /** Name of the method. */
    val msig: MethodSignature[Pattern.Ref]
) extends Symbol {
    override def toString = "MethodSymbol(%s, %x)".format(name, System.identityHashCode(this))
    
    def isFromClassNamed(aName: Name.Qual) = (clsName == aName)
    def isNamed(aName: Name.Method) = (name == aName)
    
    def methodId = Id.Method(clsName, name, msig.parameterPatterns)
    
    def modifiers(state: State) = modifiers
    
    /** For methods whose source will be emitted, we compute the 
      * overridden methods.  The ordering is significant,
      * because when super is invoked it will proceed to the
      * next implementation in the list.  For methods on
      * reflected classes or classes loaded from .class files,
      * this list is simply empty. */
    val overrides = new mutable.ListBuffer[MethodSymbol]()
}
