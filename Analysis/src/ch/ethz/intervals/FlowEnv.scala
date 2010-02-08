package ch.ethz.intervals

import scala.collection.immutable.Set
import scala.collection.immutable.ListSet
import Util._

case class FlowEnv(
    temp: Map[ir.Path, ir.Path],                // temporary equivalences, cleared on method call
    ps_invalidated: Set[ir.Path],               // p is current invalid and must be reassigned                
    readable: IntransitiveRelation[ir.Path],    // (p, q) means guard p is readable by interval q
    writable: IntransitiveRelation[ir.Path],    // (p, q) means guard p is writable by interval q
    hb: TransitiveRelation[ir.Path],            // (p, q) means interval p hb interval q
    subinterval: TransitiveRelation[ir.Path],   // (p, q) means interval p is a subinterval of interval q
    locks: IntransitiveRelation[ir.Path]        // (p, q) means interval p locks lock q            
) {
    
    def withTemp(temp: Map[ir.Path, ir.Path]) = FlowEnv(temp, ps_invalidated, readable, writable, hb, subinterval, locks)
    def withInvalidated(ps_invalidated: Set[ir.Path]) = FlowEnv(temp, ps_invalidated, readable, writable, hb, subinterval, locks)
    def withReadable(readable: IntransitiveRelation[ir.Path]) = FlowEnv(temp, ps_invalidated, readable, writable, hb, subinterval, locks)
    def withWritable(writable: IntransitiveRelation[ir.Path]) = FlowEnv(temp, ps_invalidated, readable, writable, hb, subinterval, locks)
    def withHb(hb: TransitiveRelation[ir.Path]) = FlowEnv(temp, ps_invalidated, readable, writable, hb, subinterval, locks)
    def withSubinterval(subinterval: TransitiveRelation[ir.Path]) = FlowEnv(temp, ps_invalidated, readable, writable, hb, subinterval, locks)
    def withLocks(locks: IntransitiveRelation[ir.Path]) = FlowEnv(temp, ps_invalidated, readable, writable, hb, subinterval, locks)
    
    def +(flow: FlowEnv) = {
        withTemp(temp ++ flow.temp).
        withInvalidated(ps_invalidated ++ flow.ps_invalidated)
        withReadable(readable + flow.readable).
        withWritable(writable + flow.writable).
        withHb(hb + flow.hb).
        withSubinterval(subinterval + flow.subinterval).
        withLocks(locks + flow.locks)
    }
    
    // Intersecting two environments produces an environment with the
    // "worst-case" assumptions of both.
    def **(flow2: FlowEnv) = {
        withTemp(temp ** flow2.temp).
        withInvalidated(ps_invalidated ++ flow2.ps_invalidated).
        withReadable(readable ** flow2.readable).
        withWritable(writable ** flow2.writable).
        withHb(hb ** flow2.hb).
        withSubinterval(subinterval ** flow2.subinterval).
        withLocks(locks ** flow2.locks)        
    }
    
}

object FlowEnv
{
    val empty = FlowEnv(
        Map(),
        ListSet.empty,
        IntransitiveRelation.empty,
        IntransitiveRelation.empty,
        TransitiveRelation.empty,
        TransitiveRelation.empty,
        IntransitiveRelation.empty
    )
    
    def intersect(flows: List[FlowEnv]) = flows match {
        case List() => empty
        case hd :: tl => tl.foldLeft(hd)(_ ** _)
    }    
}