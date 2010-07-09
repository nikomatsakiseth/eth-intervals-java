package harmonic.compiler.inference

import scala.collection.immutable.Set

case class EmptyFactSet[X](network: Network[X]) extends InternalFactSet[X] {
    def contains(fact: Fact): Boolean = false
    def allFactsOfKind(kind: Fact.ForwardKind): Set[Fact.Forward] = Set()
    def plusFacts(facts: Iterable[Fact], xtra: X): FactSet[X] = DerivedFactSet(this, facts, xtra)
    def resolvedAlphaMemories = Map()
    def resolvedBetaMemories = Map()
    def currentOmegaMemories = Set()
}