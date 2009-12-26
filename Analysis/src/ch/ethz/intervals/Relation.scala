package ch.ethz.intervals

import Util._

abstract class Relation[T, Self] {
    def mmap: MultiMap[T, T]
    def +(a: T, b: T): Self
    def +(rel: Self): Self        
    def -(a: T, b: T): Self    
    def empty: Self
    
    def all: Iterable[(T,T)] = mmap
    def contains(a: T, b: T) = mmap.contains(a, b)
    def apply(a: T) = mmap(a)
    def size = mmap.size
}