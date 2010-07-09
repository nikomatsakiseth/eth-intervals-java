package harmonic.compiler

sealed abstract class TcRel {
    def toFact(left: Type, right: Type): inference.Fact
    def unapply(rel: TcRel) = (rel == this)
}

case object TcEq extends TcRel {
    override def toString = ":"
    def toFact(left: Type, right: Type) = K.TypeEq(left, right)
}

case object TcSub extends TcRel {
    override def toString = "<:"
    def toFact(left: Type, right: Type) = K.TypeUb(left, right)
}

case object TcSup extends TcRel {
    override def toString = ":>"
    def toFact(left: Type, right: Type) = K.TypeUb(right, left)
}
