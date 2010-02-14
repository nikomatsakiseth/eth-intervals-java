package ch.ethz.intervals

import scala.collection.immutable.Set
import scala.collection.immutable.ListSet
import Util._

case class FlowEnv(
    nonnull: Set[ir.Path],          // paths known to be non-null
    temp: Map[ir.Path, ir.Path],    // temporary equivalences, cleared on method call
    ps_invalidated: Set[ir.Path],   // p is current invalid and must be reassigned                
    readableRel: PathRelation,      // (p, q) means guard p is readable by interval q
    writableRel: PathRelation,      // (p, q) means guard p is writable by interval q
    hbRel: PathRelation,            // (p, q) means point p hb point q
    subintervalRel: PathRelation,   // (p, q) means interval p is a subinterval of interval q
    locksRel: PathRelation          // (p, q) means interval p locks lock q            
) {
    // ___ Creating new flow environments ___________________________________
    
    def withNonnull(nonnull: Set[ir.Path]) = FlowEnv(nonnull, temp, ps_invalidated, readableRel, writableRel, hbRel, subintervalRel, locksRel)
    def withTemp(temp: Map[ir.Path, ir.Path]) = FlowEnv(nonnull, temp, ps_invalidated, readableRel, writableRel, hbRel, subintervalRel, locksRel)
    def withInvalidated(ps_invalidated: Set[ir.Path]) = FlowEnv(nonnull, temp, ps_invalidated, readableRel, writableRel, hbRel, subintervalRel, locksRel)
    def withReadableRel(readableRel: PathRelation) = FlowEnv(nonnull, temp, ps_invalidated, readableRel, writableRel, hbRel, subintervalRel, locksRel)
    def withWritableRel(writableRel: PathRelation) = FlowEnv(nonnull, temp, ps_invalidated, readableRel, writableRel, hbRel, subintervalRel, locksRel)
    def withHbRel(hbRel: PathRelation) = FlowEnv(nonnull, temp, ps_invalidated, readableRel, writableRel, hbRel, subintervalRel, locksRel)
    def withSubintervalRel(subintervalRel: PathRelation) = FlowEnv(nonnull, temp, ps_invalidated, readableRel, writableRel, hbRel, subintervalRel, locksRel)
    def withLocksRel(locksRel: PathRelation) = FlowEnv(nonnull, temp, ps_invalidated, readableRel, writableRel, hbRel, subintervalRel, locksRel)
    
    // ___ Convenience functions for taking nonnull into account ____________
    
    def mapFilter(
        rel: PathRelation,              // One of the fields of this FlowEnv
        mfunc: (ir.Path => ir.Path),    // Map to be applied
        ffunc: (ir.Path => Boolean)     // Filter to be applied after the map
    ) = {
        rel.mapFilter(mfunc, ffunc)
    }
    
    def readable = readableRel.pairs
    def writable = writableRel.pairs
    def hb = hbRel.pairs
    def subinterval = subintervalRel.pairs
    def locks = locksRel.pairs     
    
    // ___ Combining flow environments ______________________________________
    
    // Adding (or unioning) two environments keeps what's true in either.
    def +(flow: FlowEnv) = {
        withTemp(temp ++ flow.temp).
        withInvalidated(ps_invalidated ++ flow.ps_invalidated).
        withReadableRel(readableRel ++ flow.readableRel).
        withWritableRel(writableRel ++ flow.writableRel).
        withHbRel(hbRel ++ flow.hbRel).
        withSubintervalRel(subintervalRel ++ flow.subintervalRel).
        withLocksRel(locksRel ++ flow.locksRel)
    }
    
    // Intersecting two environments keeps what's true in both.
    def **(flow2: FlowEnv) = {
        withNonnull(nonnull ** flow2.nonnull)
        withTemp(temp ** flow2.temp).
        withInvalidated(ps_invalidated ++ flow2.ps_invalidated).
        withReadableRel(readableRel.intersect(flow2.readableRel)).
        withWritableRel(writableRel.intersect(flow2.writableRel)).
        withHbRel(hbRel.intersect(flow2.hbRel)).
        withSubintervalRel(subintervalRel.intersect(flow2.subintervalRel)).
        withLocksRel(locksRel.intersect(flow2.locksRel))        
    }    
}

object FlowEnv
{
    val empty = FlowEnv(
        Set(),                          // nonnull
        Map(),                          // temp
        Set(),                          // ps_invalidated
        PathRelation.intransitive,      // readableRel
        PathRelation.intransitive,      // writableRel
        PathRelation.transitive,        // hbRel
        PathRelation.transitive,        // subintervalRel
        PathRelation.intransitive       // locksRel
    )
    
    def intersect(flows: List[FlowEnv]) = flows match {
        case List() => empty
        case hd :: tl => tl.foldLeft(hd)(_ ** _)
    }    
}