package harmonic.compiler

import org.objectweb.asm.{Opcodes => O}

object FieldKind {
    case object Java extends FieldKind
    case object Harmonic extends FieldKind
}

sealed abstract class FieldKind