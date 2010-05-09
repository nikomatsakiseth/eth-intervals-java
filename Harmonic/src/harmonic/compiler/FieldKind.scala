package harmonic.compiler

import org.objectweb.asm.{Opcodes => O}

object FieldKind {
    case class Java(
        owner: java.lang.Class[_],
        name: String,
        cls: java.lang.Class[_]
    ) extends FieldKind
    case object Harmonic extends FieldKind
}

sealed abstract class FieldKind