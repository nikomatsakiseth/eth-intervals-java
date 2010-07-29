package ch.ethz.intervals.guard;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.Lock;
import ch.ethz.intervals.RoInterval;
import ch.ethz.intervals.RoLock;
import ch.ethz.intervals.RoPoint;
import ch.ethz.intervals.impl.IntervalImpl;
import ch.ethz.intervals.impl.LockImpl;
import ch.ethz.intervals.quals.Creator;
import ch.ethz.intervals.quals.GuardedBy;

/**
 * A guard is an object that defines when a field may be safely read or written.
 * There are three subtypes of {@link Guard}, each with different properties: 
 * <ol>
 * 
 * <li>{@link LockImpl}: Fields guarded by a {@link LockImpl} may only be read or written 
 * during an interval which holds the lock.  
 *  
 * <li>{@link IntervalImpl}: Fields guarded by an {@link IntervalImpl} {@code inter} may only be
 * written by the task of {@code inter}, but they
 * may be read by an interval which {@code inter} <em>happens before</em>.\
 * 
 * <li>{@link ReadTrackingDynamicGuard}: Fields guarded by a dynamic guard are dynamically monitored
 * for data-race violations.  Every write must <em>happen before</em> all later writes
 * and reads.  
 * 
 * </ol>
 *
 * <p>To indicate the guard for a field, use a {@link GuardedBy} 
 * annotation.  Note that guard annotations are only enforced when using our 
 * {@code Javac} plugin.  All guard types except {@link ReadTrackingDynamicGuard} are enforced statically; 
 * violations of {@link ReadTrackingDynamicGuard} are enforced dynamically and the required instrumentation
 * is automatically inserted by the plugin.
 * 
 * <p>If the compiler plugin cannot statically prove the safety of your program,
 * you can use the methods {@link Intervals#checkReadable(Guard)} or
 * {@link Intervals#checkWritable(Guard)} as a kind of escape clause.  Simply insert
 * an assertion like {@code assert g.checkWritable()} and the compiler
 * will respect it.  
 */
@Creator("this.(ch.ethz.intervals.quals.Constructor)")
public interface Guard {
	
	/**
	 * If fields protected by this guard are writable and/or readable by the given
	 * interval, returns {@code null}.  Otherwise, returns an exception describing
	 * the error that occurred.   
	 * 
	 * @param mr the most recent point that has already happened in {@code inter}.
	 * This is normally {@code inter.start}, but if there have been synchronous
	 * subintervals it will be the end of the most recent subinterval.
	 * 
	 * @param current the currently executing interval
	 * 
	 * @returns null if there is no error, otherwise a descriptive exception
	 */
	public RuntimeException checkWritable(RoPoint mr, RoInterval current); 

	/** 
	 * Same as {@link #checkWritable(RoPoint, RoInterval)}, but only
	 * for read access.
	 */
	public RuntimeException checkReadable(RoPoint mr, RoInterval current);
	
	/**
	 * Same as {@link #checkWritable(RoPoint, RoInterval)}, but successful
	 * only if the guard guarantees no further writes will occur.
	 */
	public RuntimeException ensuresFinal(RoPoint mr, RoInterval current);
		
	/**
	 * Checks whether fields protected by this guard may legally be locked
	 * by the lock {@code lock}.  Users would never normally need to
	 * invoke this method directly; it is invoked automatically when you
	 * use the method {@link Interval#addLock(Lock)}.
	 * 
	 * @param acq the point at which the lock was acquired.  Typically
	 * {@code interval.getStart()}.
	 * @param interval the interval that will hold the lock
	 * @param lock the lock that was acquired
	 * @returns null if there is no error, otherwise a descriptive exception
	 */
	public RuntimeException checkLockable(RoPoint acq, RoInterval interval, RoLock lock);
}
