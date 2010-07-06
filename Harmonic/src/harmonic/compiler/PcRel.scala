package harmonic.compiler

sealed abstract class PcRel {
    def is(rel: PcRel) = (rel == this)
    def unapply(rel: PcRel) = (rel == this)
}

sealed trait PcTransRel extends PcRel

sealed trait PcWcRel extends PcRel

sealed trait PcQuerySingleRel extends PcRel

sealed trait PcQueryManyRel extends PcRel

case object PcLocks extends PcQuerySpecificRel {
    override def toString = "locks"
}

case object PcSubOf extends PcTransRel with PcQueryManyRel {
    override def toString = "subOf"
}

case object PcInlineSubOf extends PcTransRel with PcQueryManyRel {
    override def toString = "inlineSubOf"
}

case object PcHb extends PcTransRel with PcQueryManyRel {
    override def toString = "->"
}

case object PcEq extends PcWcRel with PcTransRel with PcQueryManyRel {
    override def toString = "="
}

case object PcPermitsWr extends PcWcRel with PcQuerySpecificRel {
    override def toString = "permitsWr"
}

case object PcPermitsRd extends PcWcRel with PcQuerySpecificRel {
    override def toString = "permitsRd"
}

case object PcEnsuresFinal extends PcWcRel with PcQuerySpecificRel {
    override def toString = "ensuresFinal"
}
