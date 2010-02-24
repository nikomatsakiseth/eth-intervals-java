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
    inlineIntervalRel: PathRelation,   // (p, q) means interval p is a inlineInterval of interval q
    locksRel: PathRelation          // (p, q) means interval p locks lock q            
) {
    // ___ Creating new flow environments ___________________________________
    
    def withNonnull(nonnull: Set[ir.Path]) = FlowEnv(nonnull, temp, ps_invalidated, readableRel, writableRel, hbRel, inlineIntervalRel, locksRel)
    def withTemp(temp: Map[ir.Path, ir.Path]) = FlowEnv(nonnull, temp, ps_invalidated, readableRel, writableRel, hbRel, inlineIntervalRel, locksRel)
    def withInvalidated(ps_invalidated: Set[ir.Path]) = FlowEnv(nonnull, temp, ps_invalidated, readableRel, writableRel, hbRel, inlineIntervalRel, locksRel)
    def withReadableRel(readableRel: PathRelation) = FlowEnv(nonnull, temp, ps_invalidated, readableRel, writableRel, hbRel, inlineIntervalRel, locksRel)
    def withWritableRel(writableRel: PathRelation) = FlowEnv(nonnull, temp, ps_invalidated, readableRel, writableRel, hbRel, inlineIntervalRel, locksRel)
    def withHbRel(hbRel: PathRelation) = FlowEnv(nonnull, temp, ps_invalidated, readableRel, writableRel, hbRel, inlineIntervalRel, locksRel)
    def withInlineIntervalRel(inlineIntervalRel: PathRelation) = FlowEnv(nonnull, temp, ps_invalidated, readableRel, writableRel, hbRel, inlineIntervalRel, locksRel)
    def withLocksRel(locksRel: PathRelation) = FlowEnv(nonnull, temp, ps_invalidated, readableRel, writableRel, hbRel, inlineIntervalRel, locksRel)
    
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
    def inlineInterval = inlineIntervalRel.pairs
    def locks = locksRel.pairs     
    
    // ___ Combining flow environments ______________________________________
    
    // Adding (or unioning) two environments keeps what's true in either.
    def +(flow: FlowEnv) = {
        withTemp(temp ++ flow.temp).
        withInvalidated(ps_invalidated ++ flow.ps_invalidated).
        withReadableRel(readableRel ++ flow.readableRel).
        withWritableRel(writableRel ++ flow.writableRel).
        withHbRel(hbRel ++ flow.hbRel).
        withInlineIntervalRel(inlineIntervalRel ++ flow.inlineIntervalRel).
        withLocksRel(locksRel ++ flow.locksRel)
    }
    
    // Intersecting two environments keeps what's true in both.
    def &(flow2: FlowEnv) = {
        withNonnull(nonnull & flow2.nonnull)
        withTemp(temp & flow2.temp).
        withInvalidated(ps_invalidated ++ flow2.ps_invalidated).
        withReadableRel(readableRel & flow2.readableRel).
        withWritableRel(writableRel & flow2.writableRel).
        withHbRel(hbRel & flow2.hbRel).
        withInlineIntervalRel(inlineIntervalRel & flow2.inlineIntervalRel).
        withLocksRel(locksRel & flow2.locksRel)
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
        PathRelation.transitive,        // inlineIntervalRel
        PathRelation.intransitive       // locksRel
    )
    
    def intersect(flows: List[FlowEnv]) = flows match {
        case List() => empty
        case hd :: tl => tl.foldLeft(hd)(_ & _)
    }    
}