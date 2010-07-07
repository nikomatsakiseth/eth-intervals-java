package harmonic.compiler.inference

sealed trait Rule

object Rule {
    trait Forward extends Rule {
        def inputKinds: List[Fact.ForwardKind]
        def derive(state: Network#State, facts: List[Fact.Forward]): Iterable[Fact.Forward]        
    }
    
    trait Backward extends Rule {
        def outputKind: Fact.BackwardKind
        def canInfer(state: Network#State, fact: Fact.Backward): Boolean        
    }
}
