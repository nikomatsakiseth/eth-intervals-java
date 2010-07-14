package harmonic.compiler.inference

import scala.collection.immutable.Map
import scala.collection.immutable.Set

trait InternalFactSet[X] extends FactSet[X] {
    def resolvedMemories: Memories
}