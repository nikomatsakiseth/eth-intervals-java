package ch.ethz.intervals.mirror;

import ch.ethz.intervals.guard.Guard;

/** 
 * Mirror class representing intervals.
 */
public interface Interval {
	/** Returns the parent interval */
	public Interval getParent();

	/** True if this is an inline interval */
	public boolean isSynchronous();

	/** Returns the start point */
	public Point getStart();
	
	/** Returns the end point */
	public Point getEnd();
	
	/** Equivalent to {@code addLock(lock, null)} */
	public void addLock(Lock lock);
	
	/** Indicates that {@code interval} should execute while holding
	 *  the exclusive lock {@code lock}.  The lock will be automatically
	 *  acquired sometime before {@code interval.start} and release
	 *  sometime after {@code interval.end}.  You may also, optionally, 
	 *  provide a guard {@code guard} which is being protected by this lock.
	 *   
	 *  @param lock the lock to be acquired
	 *  @param guard the guard whose data is being protected, or {@code null} */
	public void addLock(Lock lock, Guard guard);
	
	/** True if this will hold {@code lock} when it executes */
	public boolean locks(Lock lock);
	
	/** Creates a new asynchronous interval child of this. */
	public AsyncInterval newAsyncChild(Task task);
	
	/** Creates a new inline interval child of this. */
	public InlineInterval newInlineChild(Task task);
}
