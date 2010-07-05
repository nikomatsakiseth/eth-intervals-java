package harmonic.compiler

sealed abstract class TcRel {
    def unapply(rel: TcRel) = (rel == this)
}

case object TcEq extends TcRel {
    override def toString = ":"
}

case object TcSub extends TcRel {
    override def toString = "<:"
}

case object TcSup extends TcRel {
    override def toString = ":>"
}
