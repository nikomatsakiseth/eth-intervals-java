package ch.ethz.intervals

import Util._

class IntransitiveRelation[T](
    private val core: MultiMap[T,T]
) extends Relation[T, IntransitiveRelation[T]] {
    def mmap = core
    def +(a: T, b: T) = new IntransitiveRelation(core + Pair(a, b))
    def +(rel: IntransitiveRelation[T]) = new IntransitiveRelation(core ++* rel.core.toMap)
    def -(a: T, b: T) = new IntransitiveRelation(core - Pair(a, b))
    def empty = IntransitiveRelation.empty
}

object IntransitiveRelation {
    def empty[T] = new IntransitiveRelation(MultiMap.empty[T,T])
}
