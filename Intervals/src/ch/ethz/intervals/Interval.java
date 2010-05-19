package ch.ethz.intervals;

import ch.ethz.intervals.guard.Guard;

/** 
 * Mirror class representing intervals.
 */
public interface Interval extends Guard {
	/** Returns the task that this interval will execute. */
	public Task getTask();
	
	/** Returns the parent interval */
	public Interval getParent();

	/** True if this is an inline interval */
	public boolean isInline();

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
	
	/**
	 * Cancels the scheduling or execution of this interval.
	 * Assuming it has not already been scheduled or executed,
	 * the interval will never be executed and no error will
	 * be generated when its parent terminates. 
	 * 
	 * @param unconditionally if false, then no error occurs
	 * if the interval has already been scheduled or executed. 
	 * 
	 * @throws IntervalException.AlreadyScheduled if
	 * <ul>
	 * <li> {@code unconditionally} is true and the interval has already been 
	 *      canceled, scheduled, or executed; or,
	 * <li> the current interval is not the interval which created {@code this}.
	 * <ul> 
	 * 
	 * @see AsyncInterval#schedule() 
	 * @see InlineInterval#execute() */
	public void cancel(boolean unconditionally);
	
}
