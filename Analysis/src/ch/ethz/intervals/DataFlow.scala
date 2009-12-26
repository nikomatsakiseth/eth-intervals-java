package ch.ethz.intervals

import scala.collection.mutable.BitSet
import scala.collection.mutable.Queue

abstract class DataFlow[R <: Relation[ir.Path, R]](
    empty: R,
    blks: Array[ir.Block], 
    gens: Array[R]
) {
    // For each block, a list of the blocks which link to us and the succ they do so with.
    val preds = Array.make(blks.length, List[(Int, ir.Succ)]())
    
    // Computed in and out sets for each block.
    val ins = Array.make(blks.length, empty)
    val outs = Array.make(blks.length, empty)
    
    // Main function:
    def compute() {
        computePreds()
        iterate()
    }
    
    // Defines the join operation:
    def join(r1: R, r2: R): R
    
    // Computes the preds array:
    private def computePreds() {
        blks.zipWithIndex.foreach { case (blk, b) => 
            blk.succs.foreach { succ => preds(succ.b) = (b, succ) :: preds(succ.b) }
        }
    }

    // Iteratively computes the in and out sets:
    private def iterate() {
    }    
    
}