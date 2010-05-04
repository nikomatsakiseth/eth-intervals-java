package harmonic.compiler

object MethodKind {
    
    // Just provides useful defaults.
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
        
}

sealed abstract class MethodKind