package harmonic.compiler

sealed abstract class Fact

object Fact {

    case class PP(left: Path.Ref, rel: PcRel, right: Path.Ref) extends Fact
    case class PT(left: Type.Ref, right: Type.Ref) extends Fact
    case class TT(left: Type.Ref, rel: TcRel, right: Type.Ref) extends Fact
    
}
