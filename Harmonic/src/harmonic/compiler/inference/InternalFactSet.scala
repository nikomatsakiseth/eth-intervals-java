package harmonic.compiler.inference

import scala.collection.immutable.Map
import scala.collection.immutable.Set

trait InternalFactSet[X] extends FactSet[X] {
    def resolvedAlphaMemories: Map[Fact.ForwardKind, Set[Fact.Forward]]
    def resolvedBetaMemories: Map[List[Fact.ForwardKind], Set[List[Fact.Forward]]]    
    def currentOmegaMemories: Set[Fact.Backward]
}