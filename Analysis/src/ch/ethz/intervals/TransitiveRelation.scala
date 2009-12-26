package ch.ethz.intervals

import Util._

class TransitiveRelation[T](
    private val core: MultiMap[T,T], 
    isCoreTC: Boolean
) extends Relation[T, TransitiveRelation[T]]
{
    lazy val tc = 
        if(!isCoreTC)
            transitiveClosure(core)
        else
            core
            
    override def mmap: MultiMap[T, T] = tc            
    def +(a: T, b: T) = new TransitiveRelation(core + Pair(a, b), false)
    def +(rel: TransitiveRelation[T]) = new TransitiveRelation(core ++* rel.core.toMap, false)
    def -(a: T, b: T) = new TransitiveRelation(tc - Pair(a, b), true)
    def empty = TransitiveRelation.empty
}

object TransitiveRelation {
    def empty[T] = new TransitiveRelation(MultiMap.empty[T,T], true)
}