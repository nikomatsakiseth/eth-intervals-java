package ch.ethz.intervals;

import ch.ethz.intervals.guard.Guard;

/** 
 * Mirror class representing intervals.
 */
public interface Interval extends RoInterval {
	/** Returns the parent interval */
	public Interval getParent();

	/** Returns the start point */
	public Point getStart();
	
	/** Returns the end point */
	public Point getEnd();
	
	/** 
	 * Equivalent to {@code addLock(getStart(), lock, lock)} 
	 * 
	 * @see #addLock(Point, Lock, Guard) */
	public void addLock(Lock lock);
	
	/** 
	 * Equivalent to {@code addLock(getStart(), lock, lock)}
	 *  
	 * @see #addLock(Point, Lock, Guard) */
	public void addLock(Lock lock, Guard guard);
	
	/** Indicates that {@code interval} should execute while holding
	 *  the exclusive lock {@code lock}.  The lock will be automatically
	 *  acquired sometime before {@code acq} and released
	 *  sometime after {@link #getEnd()}.  The purpose of the lock
	 *  is to access data guarded by {@code guard}.
	 *   
	 *  @param acq the point at which the lock is to be acquired.
	 *  Must {@link RoPoint#hbeq(RoPoint)} the start point of this
	 *  interval.
	 *  @param lock the lock to be acquired
	 *  @param guard the guard whose data is being protected 
	 *  
	 *  @see #addLock(Lock)
	 *  @see #addLock(Lock, Guard) */
	public void addLock(Point acq, Lock lock, Guard guard);
	
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
