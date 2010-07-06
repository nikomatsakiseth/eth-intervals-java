package harmonic.compiler

object Fact {

    sealed abstract class Ref
    case class PP(left: Path.Ref, rel: PcRel, right: Path.Ref) extends Ref
    case class PC(left: Type.Ref, right: Type.Class) extends Ref
    case class TT(left: Type.Ref, rel: TcRel, right: Type.Ref) extends Ref
    
}
