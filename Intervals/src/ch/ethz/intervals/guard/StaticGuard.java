package ch.ethz.intervals.guard;

/**
 * A marker interface indicating that this guard class is intended
 * to be checked statically by the Harmonic compiler.  Such guard classes 
 * must meet several conditions:
 * <ul>
 * 
 * <li> It must be possible to create instances of them
 *      from within the compiler, so their data dependencies
 *      on the rest of the system must be minimal.
 *      
 * <li> Their checks must be <i>monotonic</i>, meaning that
 *      an interval which is permitted access under schedule X
 *      would never be denied under another schedule Y containing
 *      more constraints (i.e., hb edges, locks) than X.
 *      In other words, the check must never look for the <b>absence</b>
 *      of a <i>happens-before</i> edge or a lock, only for their
 *      <b>presence</b>. 
 *      
 *      The reason for this is that the compiler will be invoking the 
 *      {@link Guard} methods at compilation time using mock 
 *      intervals and locks.  These intervals and locks will only
 *      reflect a <B>portion</B> of the final schedule, not the
 *      complete schedule.  So if they say that point P does not
 *      <i>happen before</i> Q, it may happen at runtime that P in fact
 *      DOES <i>happen before</i> Q, but the compiler could not prove it.
 *      Therefore, the check which succeeded at compilation time
 *      (because P did not HB Q) would fail at runtime.  That's bad.
 * </ul>
 */
public interface StaticGuard extends Guard {

}
