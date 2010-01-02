package ch.ethz.intervals

import Util._

abstract class Relation[T, R <: Relation[T, R]] 
extends Iterable[(T, T)] {
    def mmap: MultiMap[T, T]
    def +(a: T, b: T): R
    def +(rel: R): R        
    def -(a: T, b: T): R    
    
    def +(pair: (T, T)): R = this + (pair._1, pair._2)
    
    def empty: R
    def elements: Iterator[(T,T)] = mmap.elements
    def contains(a: T, b: T): Boolean = mmap.contains(a, b)
    def apply(a: T) = mmap(a)
    def size = mmap.size
    def **(rel: R) =
        elements.foldLeft(empty) { case (r, (a, b)) => 
            if(rel.contains(a, b)) r + (a, b)
            else r
        }
        
    def mapFilter(
        mapFunc: (T => T), 
        filterFunc: (T => Boolean)
    ): R =
        elements.foldLeft(empty) { case (r, (a, b)) =>
            val a1 = mapFunc(a)
            val b1 = mapFunc(b)
            if(filterFunc(a1) && filterFunc(b1)) r + (a1, b1)
            else r
        }        
        
    override def equals(that: Any) = 
        if(!that.isInstanceOf[Relation[_, _]])
            false
        else {
            Set(elements.toList: _*) == Set(that.asInstanceOf[Relation[_, _]].elements.toList: _*)
        }
        
}