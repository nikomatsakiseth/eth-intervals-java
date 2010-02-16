package ch.ethz.intervals

import scala.collection.immutable.Map
import scala.collection.immutable.Set

case class MultiMap[K, V](
    override val size: Int,
    map: Map[K, Set[V]]
) extends Set[(K, V)] {
    
    def empty[B] = Set() // not really relevant here.
    
    def +(pair: (K, V)): MultiMap[K, V] = {
        if(contains(pair))
            this
        else
            MultiMap(size + 1, map + Pair(pair._1, values(pair._1) + pair._2))
    }
    
    def plusMap(mmap: MultiMap[K, V]) = {
        mmap.map.iterator.foldLeft(this) { case (m, (k, vs)) =>
            m.plusMany(k, vs)
        }
    }
    
    def plusMany(k: K, v: Iterable[V]) = {
        val set_before = values(k)
        val set_after = values(k) ++ v
        MultiMap(size + set_after.size - set_before.size, map + Pair(k, set_after))
    }
    
    def -(pair: (K, V)): MultiMap[K, V] = {
        if(contains(pair)) {
            val set = values(pair._1) - pair._2
            if(set.isEmpty)
                MultiMap(size - 1, map - pair._1)
            else
                MultiMap(size - 1, map + Pair(pair._1, set))            
        } else {
            this
        }
    }
    
    override def filter(func: (Pair[K, V] => Boolean)): MultiMap[K, V] = {
        foldLeft(this) { case (mmap, pair) =>
            if(func(pair)) mmap
            else mmap - pair
        }
    }
    
    def contains(pair: (K, V)) = values(pair._1)(pair._2)
    
    def keysIterator = map.keysIterator
    
    def keySet = map.keySet

    def values(key: K): Set[V] = {
        map.get(key) match {
            case Some(s) => s
            case None => Set()
        }
    }
    
    def iterator = for(k <- map.keysIterator; v <- map(k).iterator) yield (k, v)
    
}

object MultiMap {
    
    def empty[K,V] = MultiMap[K,V](0, Map())

}