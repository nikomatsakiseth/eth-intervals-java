package ch.ethz.intervals;

import ch.ethz.intervals.params.Parent;

@Parent
public interface Interval {
	
	/** @return The start point for this interval. */
	public Point start();
	
	/** @return The end point for this interval. */
	public Point end();
	
	/** Short for {@code end().bound()} */
	public Point bound();
	
}
