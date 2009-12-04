package ch.ethz.intervals;

/** The span of time required to execute its associated task and any
 *  subtasks. */
public interface Interval {
	
	/** @return The start point for this interval. */
	public Point start();
	
	/** @return The end point for this interval. */
	public Point end();
	
	/** Short for {@code end().bound()} */
	public Point bound();
	
	/** Makes this interval eligible for execution.  This method
	 *  can only be invoked from within the task that created the
	 *  receiver.
	 *  
	 *  @throws AlreadyScheduledException if the receiver is not
	 *  on the list of unscheduled exceptions for the current
	 *  interval.  This generally indicates the interval
	 *  was already scheduled, although it may have been created
	 *  in another interval as well. */
	public void schedule() throws AlreadyScheduledException;
	
}
