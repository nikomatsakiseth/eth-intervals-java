package ch.ethz.intervals

import scala.collection.immutable.Set
import scala.collection.immutable.ListSet
import Util._

case class FlowEnv(
    nonnull: Set[ir.Path],          // paths known to be non-null
    temp: Map[ir.Path, ir.Path],    // temporary equivalences, cleared on method call
    ps_invalidated: Set[ir.Path],   // p is current invalid and must be reassigned                
    equivRel: PathRelation,         // (p, q) means p == q
    readableRel: PathRelation,      // (p, q) means guard p is readable by interval q
    writableRel: PathRelation,      // (p, q) means guard p is writable by interval q
    hbRel: PathRelation,            // (p, q) means point p hb point q
    inlineRel: PathRelation,        // (p, q) means interval p is a inlineInterval of interval q
    locksRel: PathRelation          // (p, q) means interval p locks lock q            
) {
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
    def inline = inlineRel.pairs
    def locks = locksRel.pairs     
    
    // ___ Combining flow environments ______________________________________
    
    // Adding (or unioning) two environments keeps what's true in either.
    def +(flow: FlowEnv) = copy(
        temp = temp ++ flow.temp,
        ps_invalidated = ps_invalidated ++ flow.ps_invalidated,
        readableRel = readableRel ++ flow.readableRel,
        writableRel = writableRel ++ flow.writableRel,
        hbRel = hbRel ++ flow.hbRel,
        inlineRel = inlineRel ++ flow.inlineRel,
        locksRel = locksRel ++ flow.locksRel
    )
    
    // Intersecting two environments keeps what's true in both.
    def &(flow2: FlowEnv) = copy(
        nonnull = nonnull & flow2.nonnull,
        temp = temp & flow2.temp,
        ps_invalidated = ps_invalidated ++ flow2.ps_invalidated,
        readableRel = readableRel & flow2.readableRel,
        writableRel = writableRel & flow2.writableRel,
        hbRel = hbRel & flow2.hbRel,
        inlineRel = inlineRel & flow2.inlineRel,
        locksRel = locksRel & flow2.locksRel
    )
}

object FlowEnv
{
    val empty = FlowEnv(
        nonnull = Set(),
        temp = Map(),
        ps_invalidated = Set(),
        equivRel = PathRelation.transitive,
        readableRel = PathRelation.intransitive,
        writableRel = PathRelation.intransitive,
        hbRel = PathRelation.transitive,
        inlineRel = PathRelation.transitive,
        locksRel = PathRelation.intransitive
    )
    
    def intersect(flows: List[FlowEnv]) = flows match {
        case List() => empty
        case hd :: tl => tl.foldLeft(hd)(_ & _)
    }    
}