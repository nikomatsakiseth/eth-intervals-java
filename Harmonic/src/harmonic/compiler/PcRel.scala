package harmonic.compiler

sealed abstract class PcRel {
    def is(rel: PcRel) = (rel == this)
    def unapply(rel: PcRel) = (rel == this)
    def toFact(left: Path, right: Path): inference.Fact
    
    def classes: (Name.Class, Name.Class)
}

sealed trait PcWcRel extends PcRel {
    def toFact(left: Path, right: Path): inference.Fact    
}

sealed trait PcForwardRel extends PcRel {
    def toFact(left: Path, right: Path): inference.Fact.Forward
}

sealed trait PcTransRel extends PcForwardRel

sealed trait PcBackwardRel extends PcRel {
    def toFact(left: Path, right: Path): inference.Fact.Backward
}

case object PcLocks extends PcForwardRel {
    override def toString = "locks"
    def toFact(left: Path, right: Path) = K.Locks(left, right)
    def classes = (Name.RoIntervalClass, Name.RoLockClass)
}

case object PcSubOf extends PcTransRel {
    override def toString = "subOf"
    def toFact(left: Path, right: Path) = K.SubOf(left, right)
    def classes = (Name.RoIntervalClass, Name.RoIntervalClass)
}

case object PcInlineSubOf extends PcTransRel {
    override def toString = "inlineSubOf"
    def toFact(left: Path, right: Path) = K.InlineSubOf(left, right)
    def classes = (Name.RoIntervalClass, Name.RoIntervalClass)
}

case object PcHb extends PcTransRel {
    override def toString = "->"
    def toFact(left: Path, right: Path) = K.Hb(left, right)
    def classes = (Name.RoPointClass, Name.RoPointClass)
}

case object PcEq extends PcWcRel with PcTransRel {
    override def toString = "="
    def toFact(left: Path, right: Path) = K.PathEq(left, right)
    def classes = (Name.ObjectClass, Name.ObjectClass)
}

sealed trait PcGuardRel extends PcBackwardRel {
    def classes = (Name.GuardClass, Name.RoIntervalClass)    
}

case object PcPermitsWr extends PcWcRel with PcGuardRel {
    override def toString = "permitsWr"
    def toFact(left: Path, right: Path) = K.PermitsWr(left, right)
}

case object PcPermitsRd extends PcWcRel with PcGuardRel {
    override def toString = "permitsRd"
    def toFact(left: Path, right: Path) = K.PermitsRd(left, right)
}

case object PcEnsuresFinal extends PcWcRel with PcGuardRel {
    override def toString = "ensuresFinal"
    def toFact(left: Path, right: Path) = K.EnsuresFinal(left, right)
}
