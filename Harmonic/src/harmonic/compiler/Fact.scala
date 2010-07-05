package harmonic.compiler

object Fact {

    sealed abstract class Ref
    case class PathPath(left: Path.Ref, rel: PcRel, right: Path.Ref) extends Ref
    case class PathClass(left: Type.Ref, right: Type.Class) extends Ref
    case class TypeType(left: Type.Ref, rel: TcRel, right: Type.Ref) extends Ref
    
}
