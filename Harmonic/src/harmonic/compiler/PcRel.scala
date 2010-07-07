package harmonic.compiler

sealed abstract class PcRel {
    def is(rel: PcRel) = (rel == this)
    def unapply(rel: PcRel) = (rel == this)
}

sealed trait PcWcRel extends PcRel

sealed trait PcForwardRel extends PcRel

sealed trait PcTransRel extends PcForwardRel

sealed trait PcBackwardRel extends PcRel

case object PcLocks extends PcRel with PcBackwardRel {
    override def toString = "locks"
}

case object PcSubOf extends PcTransRel {
    override def toString = "subOf"
}

case object PcInlineSubOf extends PcTransRel {
    override def toString = "inlineSubOf"
}

case object PcHb extends PcTransRel {
    override def toString = "->"
}

case object PcEq extends PcWcRel with PcTransRel {
    override def toString = "="
}

case object PcPermitsWr extends PcWcRel with PcBackwardRel {
    override def toString = "permitsWr"
}

case object PcPermitsRd extends PcWcRel with PcBackwardRel {
    override def toString = "permitsRd"
}

case object PcEnsuresFinal extends PcWcRel with PcBackwardRel {
    override def toString = "ensuresFinal"
}
