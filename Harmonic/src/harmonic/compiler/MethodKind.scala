package harmonic.compiler

import org.objectweb.asm.{Opcodes => O}

object MethodKind {
    
    sealed abstract class JavaOpcode(val op: Int)
    case object JavaVirtual extends JavaOpcode(O.INVOKEVIRTUAL)
    case object JavaInterface extends JavaOpcode(O.INVOKEINTERFACE)
    case object JavaStatic extends JavaOpcode(O.INVOKESTATIC)
    case object JavaSpecial extends JavaOpcode(O.INVOKESPECIAL)
    
    sealed abstract class Harmonic(val op: Int) extends MethodKind
    case object HarmonicVirtual extends Harmonic(O.INVOKEINTERFACE)
    case object HarmonicCtor extends Harmonic(O.INVOKESPECIAL)
    
    case class Java(
        op: JavaOpcode,
        ownerClass: java.lang.Class[_],
        mthdName: String,
        argumentClasses: Array[java.lang.Class[_]],
        resultClass: java.lang.Class[_]
    ) extends MethodKind
    
    case object JavaDummyCtor extends MethodKind
    
    case object ErrorMethod extends MethodKind
        
}

sealed abstract class MethodKind