package ch.ethz.intervals

import scala.collection.immutable.Set
import scala.collection.immutable.ListSet
import Util._

case class FlowEnv(
    nonnull: Set[ir.Path],          // paths known to be non-null
    temp: Map[ir.Path, ir.Path],    // temporary equivalences, cleared on method call
    ps_invalidated: Set[ir.Path],   // p is current invalid and must be reassigned                
    readable: PathRelation,         // (p, q) means guard p is readable by interval q
    writable: PathRelation,         // (p, q) means guard p is writable by interval q
    hb: PathRelation,               // (p, q) means interval p hb interval q
    subinterval: PathRelation,      // (p, q) means interval p is a subinterval of interval q
    locks: PathRelation             // (p, q) means interval p locks lock q            
) {
    // ___ Creating new flow environments ___________________________________
    
    def withNonnull(nonnull: Set[ir.Path]) = FlowEnv(nonnull, temp, ps_invalidated, readable, writable, hb, subinterval, locks)
    def withTemp(temp: Map[ir.Path, ir.Path]) = FlowEnv(nonnull, temp, ps_invalidated, readable, writable, hb, subinterval, locks)
    def withInvalidated(ps_invalidated: Set[ir.Path]) = FlowEnv(nonnull, temp, ps_invalidated, readable, writable, hb, subinterval, locks)
    def withReadable(readable: PathRelation) = FlowEnv(nonnull, temp, ps_invalidated, readable, writable, hb, subinterval, locks)
    def withWritable(writable: PathRelation) = FlowEnv(nonnull, temp, ps_invalidated, readable, writable, hb, subinterval, locks)
    def withHb(hb: PathRelation) = FlowEnv(nonnull, temp, ps_invalidated, readable, writable, hb, subinterval, locks)
    def withSubinterval(subinterval: PathRelation) = FlowEnv(nonnull, temp, ps_invalidated, readable, writable, hb, subinterval, locks)
    def withLocks(locks: PathRelation) = FlowEnv(nonnull, temp, ps_invalidated, readable, writable, hb, subinterval, locks)
    
    // ___ Convenience functions for taking nonnull into account ____________
    
    def mapFilter(
        rel: PathRelation,              // One of the fields of this FlowEnv
        mfunc: (ir.Path => ir.Path),    // Map to be applied
        ffunc: (ir.Path => Boolean)     // Filter to be applied after the map
    ) = {
        rel.mapFilter(mfunc, ffunc)
    }
    
    def readablePairs = readable.pairs
    def writablePairs = writable.pairs
    def hbPairs = hb.pairs
    def subintervalPairs = subinterval.pairs
    def locksPairs = locks.pairs     
    
    def superintervals(p: ir.Path) = {
        subinterval.values(p)
    }        
    
    // ___ Combining flow environments ______________________________________
    
    // Adding (or unioning) two environments keeps what's true in either.
    def +(flow: FlowEnv) = {
        withTemp(temp ++ flow.temp).
        withInvalidated(ps_invalidated ++ flow.ps_invalidated).
        withReadable(readable ++ flow.readable).
        withWritable(writable ++ flow.writable).
        withHb(hb ++ flow.hb).
        withSubinterval(subinterval ++ flow.subinterval).
        withLocks(locks ++ flow.locks)
    }
    
    // Intersecting two environments keeps what's true in both.
    def **(flow2: FlowEnv) = {
        withNonnull(nonnull ** flow2.nonnull)
        withTemp(temp ** flow2.temp).
        withInvalidated(ps_invalidated ++ flow2.ps_invalidated).
        withReadable(readable.intersect(flow2.readable)).
        withWritable(writable.intersect(flow2.writable)).
        withHb(hb.intersect(flow2.hb)).
        withSubinterval(subinterval.intersect(flow2.subinterval)).
        withLocks(locks.intersect(flow2.locks))        
    }    
}

object FlowEnv
{
    val empty = FlowEnv(
        Set(),                          // nonnull
        Map(),                          // temp
        Set(),                          // ps_invalidated
        PathRelation.intransitive,      // readable
        PathRelation.intransitive,      // writable
        PathRelation.transitive,        // hb
        PathRelation.transitive,        // subinterval
        PathRelation.intransitive       // locks
    )
    
    def intersect(flows: List[FlowEnv]) = flows match {
        case List() => empty
        case hd :: tl => tl.foldLeft(hd)(_ ** _)
    }    
}