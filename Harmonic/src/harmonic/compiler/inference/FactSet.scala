package harmonic.compiler.inference

import scala.collection.immutable

trait FactSet[X] {
    def network: Network[X]
    
    /** Ask after a specific fact, either forward or backward */
    def contains(fact: Fact): Boolean

    /** Query multiple forward facts.  You may supply a number of arguments
      * up to the arity of F.  If the argument is not None, then 
      * only facts which match will be returned. */
    def query[F <: Fact.Forward](
        kind: Class[F], 
        optArgs: Option[Any]*
    ): Set[F]
    
    /** Specialized variant for binary facts:
      * Extracts `right` given `left`. */
    def queryRGivenL[L, R, F <: Fact.Binary[L, R]](
        kind: Class[F], 
        left: L
    ): Set[R] = {
        query(kind, Some(left)).map(_.right)
    }

    /** Specialized variant for binary facts:
      * Extracts `left` given `right`. */
    def queryLGivenR[L, R, F <: Fact.Binary[L, R]](
        kind: Class[F], 
        right: R
    ): Set[L] = {
        query(kind, None, Some(right)).map(_.left)
    }

    /** Adds facts, returning a new fact set. */
    def plusFacts(facts: Iterable[Fact], xtra: X): FactSet[X]
    
    /** Adds all facts from the given fact set, returning a new fact set. */
    def plusFactSet(factSet: FactSet[X], xtra: X): FactSet[X] // must be from same network
}