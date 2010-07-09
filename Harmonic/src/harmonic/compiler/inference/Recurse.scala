package harmonic.compiler.inference

/** Interface that backwards rules can use to recursively query. */
trait Recurse[X] {
    def xtra: X
    def contains(fact: Fact): Boolean
    def allFactsOfKind(kind: Fact.ForwardKind): Set[Fact.Forward]
}