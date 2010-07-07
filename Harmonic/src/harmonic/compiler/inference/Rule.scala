package harmonic.compiler.inference

sealed trait Rule

object Rule {
    trait Forward extends Rule {
        def inputKinds: Array[Fact.ForwardKind]
        def derive(state: Network.State, facts: Array[Fact.Forward]): Iterable[Fact]        
    }
    
    trait Backward extends Rule {
        def outputKind: Fact.BackwardKind
        def canInfer(state: Network.State, fact: Fact.Backward): Boolean        
    }
}
