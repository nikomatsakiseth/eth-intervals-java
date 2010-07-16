package harmonic.compiler.inference

/** Interface that backwards rules can use to recursively query. */
trait Recurse[X] {
    def xtra: X
    
    def contains(fact: Fact): Boolean

    def query[F <: Fact.Forward](
        kind: Class[F], 
        optArgs: Option[Any]*
    ): Set[F]
    
    def queryRGivenL[L, R](
        left: L,
        kind: Class[_ <: Fact.Binary[L, R]]
    ): Set[R] = {
        query(kind, Some(left)).map(_.right)
    }

    def queryLGivenR[L, R](
        kind: Class[_ <: Fact.Binary[L, R]], 
        right: R
    ): Set[L] = {
        query(kind, None, Some(right)).map(_.left)
    }
}