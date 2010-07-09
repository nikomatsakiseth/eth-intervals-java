package harmonic.compiler.inference

import scala.collection.immutable

trait FactSet[X] {
    def network: Network[X]
    
    def contains(fact: Fact): Boolean

    def allFactsOfKind(kind: Fact.ForwardKind): Set[Fact.Forward]

    def plusFacts(facts: Iterable[Fact], xtra: X): FactSet[X]
    
    def plusFactSet(factSet: FactSet[X], xtra: X): FactSet[X] // must be from same network
}