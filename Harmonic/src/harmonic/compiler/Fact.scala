package harmonic.compiler

sealed abstract class Fact

object Fact {
    // PRP facts encode relations between paths.
    //
    // Example: guard permitsWr inter
    case class PRP(left: Path.Ref, rel: PcRel, right: Path.Ref) extends Fact
    
    // TRT facts encode bounds and equality relations between types.
    //
    // Example: path.X <: C[...]
    //          C[X permitsWr path] <: C[X permitsRd path]
    case class TRT(left: Type, rel: TcRel, right: Type) extends Fact
    
    // Encodes fact that value at `path` can be assigned to an
    // lvalue of type `ty`.
    //
    // Example: ... fairly clear, I hope ...
    case class Assignable(path: Path.Ref, ty: Type) extends Fact
}
