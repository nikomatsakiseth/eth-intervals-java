package ch.ethz.intervals;

public interface Point {
	
	/**
	 * Returns the bound of this point.  For a start point, the 
	 * bound is the corresponding end point.  For an end point,
	 * the bound is either the end of the root interval, or
	 * specified by the user when using 
	 * {@link Intervals#intervalWithBound(Point)}. 
	 * 
	 * There is always an outgoing edge from every point to its
	 * bound.  Bounds are also important in dynamic race detection.
	 */
	Point bound();	
	
	/**
	 * True if this point is bounded by {@code p} or by some point which
	 * is in turn bounded by {@code p}.
	 */
	boolean isBoundedBy(Point p);
	
	/**
	 * True if this point <i>happens before</i> {@code p}.  Which
	 * points <i>happen before</i> one another is specified by
	 * the user when new intervals are created via methods like
	 * {@link UnscheduledInterval#endAfter(Point)} or
	 * {@link UnscheduledInterval#startBefore(Point)}.
	 */
	boolean hb(Point p);

}
