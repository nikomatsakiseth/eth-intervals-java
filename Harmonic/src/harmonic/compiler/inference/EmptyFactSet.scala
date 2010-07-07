package harmonic.compiler.inference

import scala.collection.immutable.Set

case class EmptyFactSet(network: Network) extends InternalFactSet {
    def contains(fact: Fact): Boolean = false
    def allFactsOfKind(kind: Fact.ForwardKind): Set[Fact.Forward] = Set()
    def plusFacts(facts: Iterable[Fact]): FactSet = DerivedFactSet(this, facts)
    def resolvedAlphaMemories = Map()
    def resolvedBetaMemories = Map()
    def currentOmegaMemories = Set()
}