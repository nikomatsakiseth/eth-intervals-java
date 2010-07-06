package harmonic.compiler

sealed abstract class Fact

object Fact {

    case class PP(left: Path.Ref, rel: PcRel, right: Path.Ref) extends Fact
    case class TT(left: Type, rel: TcRel, right: Type) extends Fact
    
}
