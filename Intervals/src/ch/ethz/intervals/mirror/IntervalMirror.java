package ch.ethz.intervals.mirror;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.guard.Guard;

/** 
 * Mirror class representing intervals.  {@link Guard} implementations should
 * use this class in place of {@link Interval}.
 */
public interface IntervalMirror {
	public IntervalMirror parent();
	public boolean isSynchronous();
	public PointMirror start();
	public PointMirror end();
	public void addLock(LockMirror lock, Guard guard);
	public boolean locks(LockMirror lock);
}
