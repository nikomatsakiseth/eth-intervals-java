package harmonic.compiler.inference

case class Memories(
    alphas: Map[Fact.ForwardKind, Set[Fact.Forward]],
    betas: Map[List[Fact.ForwardKind], Set[List[Fact.Forward]]],
    omegas: Set[Fact.Backward]
)