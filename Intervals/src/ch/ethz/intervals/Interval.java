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
	
}
