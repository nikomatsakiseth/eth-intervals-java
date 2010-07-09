package harmonic.compiler

sealed abstract class PcRel {
    def is(rel: PcRel) = (rel == this)
    def unapply(rel: PcRel) = (rel == this)
    def toFact(left: Path.Ref, right: Path.Ref): inference.Fact
}

sealed trait PcWcRel extends PcRel {
    def toFact(left: Path.Ref, right: Path.Ref): inference.Fact    
}

sealed trait PcForwardRel extends PcRel {
    def toFact(left: Path.Ref, right: Path.Ref): inference.Fact.Forward
}

sealed trait PcTransRel extends PcForwardRel

sealed trait PcBackwardRel extends PcRel {
    def toFact(left: Path.Ref, right: Path.Ref): inference.Fact.Backward
}

case object PcLocks extends PcRel with PcForwardRel {
    override def toString = "locks"
    def toFact(left: Path.Ref, right: Path.Ref) = K.Locks(left, right)
}

case object PcSubOf extends PcTransRel {
    override def toString = "subOf"
    def toFact(left: Path.Ref, right: Path.Ref) = K.SubOf(left, right)
}

case object PcInlineSubOf extends PcTransRel {
    override def toString = "inlineSubOf"
    def toFact(left: Path.Ref, right: Path.Ref) = K.InlineSubOf(left, right)
}

case object PcHb extends PcTransRel {
    override def toString = "->"
    def toFact(left: Path.Ref, right: Path.Ref) = K.Hb(left, right)
}

case object PcEq extends PcWcRel with PcTransRel {
    override def toString = "="
    def toFact(left: Path.Ref, right: Path.Ref) = K.PathEq(left, right)
}

case object PcPermitsWr extends PcWcRel with PcBackwardRel {
    override def toString = "permitsWr"
    def toFact(left: Path.Ref, right: Path.Ref) = K.PermitsWr(left, right)
}

case object PcPermitsRd extends PcWcRel with PcBackwardRel {
    override def toString = "permitsRd"
    def toFact(left: Path.Ref, right: Path.Ref) = K.PermitsRd(left, right)
}

case object PcEnsuresFinal extends PcWcRel with PcBackwardRel {
    override def toString = "ensuresFinal"
    def toFact(left: Path.Ref, right: Path.Ref) = K.EnsuresFinal(left, right)
}
