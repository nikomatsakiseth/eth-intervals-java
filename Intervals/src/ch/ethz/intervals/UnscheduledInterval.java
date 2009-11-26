package ch.ethz.intervals;

public interface UnscheduledInterval {
	
	/** 
	 * Adds a happens-before edge from the start of this interval to {@code p}. 
	 */
	UnscheduledInterval startAfter(Point p);
	
	/** 
	 * Adds a happens-before edge from {@code p} to the start of this interval. 
	 */
	UnscheduledInterval startBefore(Point p);
	
	/** 
	 * Adds a happens-before edge from the end of this interval to {@code p}. 
	 */
	UnscheduledInterval endAfter(Point p);
	
	/** 
	 * Adds a happens-before edge from {@code p} to the end of this interval. 
	 */
	UnscheduledInterval endBefore(Point p);
	
	/** 
	 * If {@code maskExceptions} is false, then any runtime error will be
	 * propagated to the interval's bound.  If true, then runtime errors
	 * are not propagated.  The default value is false.
	 */
	UnscheduledInterval setMaskExceptions(boolean maskExceptions);

	/** 
	 * Indicates that an exclusive lock on {@code g} must be acquired before
	 * this interval can execute and released once this interval has finished.
	 */
	UnscheduledInterval exclusiveLock(Guard g);
	
	/** 
	 * Schedules the interval for execution and returns the scheduled version.
	 */
	Interval schedule(Task task);

}
