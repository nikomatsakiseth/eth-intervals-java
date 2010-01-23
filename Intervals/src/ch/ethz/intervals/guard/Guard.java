package ch.ethz.intervals.guard;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.Lock;
import ch.ethz.intervals.mirror.IntervalMirror;
import ch.ethz.intervals.mirror.LockMirror;
import ch.ethz.intervals.mirror.PointMirror;
import ch.ethz.intervals.quals.Creator;
import ch.ethz.intervals.quals.GuardedBy;
import ch.ethz.intervals.quals.WrittenDuring;

/**
 * A guard is an object that defines when a field may be safely read or written.
 * There are three subtypes of {@link Guard}, each with different properties: 
 * <ol>
 * 
 * <li>{@link Lock}: Fields guarded by a {@link Lock} may only be read or written 
 * during an interval which holds the lock.  
 *  
 * <li>{@link Interval}: Fields guarded by an {@link Interval} {@code inter} may only be
 * written by the {@link Interval#run()} method of {@code inter}, but they
 * may be read by an interval which {@code inter} <em>happens before</em>.\
 * 
 * <li>{@link DefaultDynamicGuard}: Fields guarded by a dynamic guard are dynamically monitored
 * for data-race violations.  Every write must <em>happen before</em> all later writes
 * and reads.  
 * 
 * </ol>
 *
 * <p>To indicate the guard for a field, use a {@link GuardedBy} or {@link WrittenDuring}
 * annotation.  Note that guard annotations are only enforced when using our 
 * {@code Javac} plugin.  All guard types except {@link DefaultDynamicGuard} are enforced statically; 
 * violations of {@link DefaultDynamicGuard} are enforced dynamically and the required instrumentation
 * is automatically inserted by the plugin.
 * 
 * <p>If the compiler plugin cannot statically prove the safety of your program,
 * you can use the methods {@link Intervals#checkReadable(Guard)} or
 * {@link Intervals#checkWritable(Guard)} as a kind of escape clause.  Simply insert
 * an assertion like {@code assert g.checkWritable()} and the compiler
 * will respect it.  
 */
@Creator("this.constructor")
public interface Guard {
	
	/**
	 * If fields protected by this guard are writable and/or readable by the given
	 * interval, returns {@code null}.  Otherwise, returns an exception describing
	 * the error that ocurred.   
	 * 
	 * @param mr the most recent point that has already happened in {@code inter}.
	 * This is normally {@code inter.start}, but if there have been synchronous
	 * subintervals it will be the end of the most recent subinterval.
	 * 
	 * @param inter the currently executing interval
	 * 
	 * @returns null if there is no error, otherwise a descriptive exception
	 */
	public RuntimeException checkWritable(PointMirror mr, IntervalMirror inter); 

	/** 
	 * Same as {@link #checkWritable(PointMirror, IntervalMirror)}, but only
	 * for read access.
	 */
	public RuntimeException checkReadable(PointMirror mr, IntervalMirror current);
		
	/**
	 * Checks whether fields protected by this guard may legally be locked
	 * by the lock {@code lock}.  Users would never normally need to
	 * invoke this method directly; it is invoked automatically when you
	 * use the method {@link Intervals#addExclusiveLock(Interval, Lock)}.
	 * 
	 * @returns null if there is no error, otherwise a descriptive exception
	 */
	public RuntimeException checkLockable(IntervalMirror interval, LockMirror lock);
}
