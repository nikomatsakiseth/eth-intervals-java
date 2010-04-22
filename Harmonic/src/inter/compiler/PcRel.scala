package inter.compiler

sealed abstract class PcRel

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

case object PcPermitsWr extends PcWcRel {
    override def toString = "permitsWr"
}

case object PcPermitsRd extends PcWcRel {
    override def toString = "permitsRd"
}
