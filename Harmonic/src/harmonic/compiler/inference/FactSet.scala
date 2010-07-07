package harmonic.compiler.inference

import scala.collection.immutable

trait FactSet {
    def network: Network
    
    def contains(fact: Fact): Boolean

    def allFactsOfKind(kind: Fact.ForwardKind): Set[Fact.Forward]

    def plusFacts(facts: Iterable[Fact]): FactSet
}