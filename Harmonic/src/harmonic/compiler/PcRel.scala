package harmonic.compiler

sealed abstract class PcRel {
    def unapply(rel: PcRel) = (rel == this)
}

case object PcLocks extends PcRel {
    override def toString = "locks"
}

case object PcSubOf extends PcRel {
    override def toString = "subOf"
}

case object PcInlineSubOf extends PcRel {
    override def toString = "inlineSubOf"
}

sealed abstract class PcTransRel extends PcRel

case object PcHb extends PcTransRel {
    override def toString = "->"
}

sealed abstract class PcWcRel extends PcRel

case object PcEq extends PcWcRel {
    override def toString = "="
}

/** Guard permits writes by interval */
case object PcPermitsWr extends PcWcRel {
    override def toString = "permitsWr"
}

/** Guard permits reads by interval */
case object PcPermitsRd extends PcWcRel {
    override def toString = "permitsRd"
}

/** Guard ensures final by interval */
case object PcEnsuresFinal extends PcWcRel {
    override def toString = "ensuresFinal"
}
