package ch.ethz.intervals;

import ch.ethz.intervals.params.Parent;

@Parent
public interface Interval<R> {
	
	/** @return The start point for this interval. */
	public StartPoint start();
	
	/** @return The end point for this interval. */
	public EndPoint<R> end();
	
	/** Short for {@code end().bound()} */
	public Point bound(); 

}
