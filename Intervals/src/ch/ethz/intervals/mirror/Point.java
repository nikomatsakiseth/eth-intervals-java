package ch.ethz.intervals.mirror;

import ch.ethz.intervals.CycleException;
import ch.ethz.intervals.EdgeNeededException;
import ch.ethz.intervals.guard.Guard;
import ch.ethz.intervals.impl.PointImpl;

/** 
 * Mirror class representing points.  {@link Guard} implementations should
 * use this class in place of {@link PointImpl}.
 */
public interface Point {
	
	/** 
	 * Returns the bound of this point. */
	public Point bound();
	
	/** 
	 * Returns an array {@code bounds} where 
	 * {@code bounds[0]} == end of the root interval
	 * code the last element is {@code this}. */
	public Point[] getBounds();
	
	/**
	 * Returns the mutual bound of {@code this} and
	 * {@code pnt}. */
	public Point mutualBound(Point pnt);
	
	public boolean isBoundedBy(Point pnt);
	
	public boolean hb(Point pnt);
	
	public boolean hbeq(Point pnt);
	
	/** 
	 * Creates a dependency so that {@code this} <em>happens before</em>
	 * {@code to}.  
	 * 
	 * Invoking this method is only legal when one of the following
	 * conditions is true, as they ensure that {@code to}
	 * cannot have occurred yet:
	 * <ul>
	 * <li>{@code to} belongs to an unscheduled interval creates by
	 *     the current task.
	 * <li>{@code to} is the end of the current interval.
	 * <li>The end of the current interval <em>happens before</em> {@code to}.
	 * </ul>
	 * If none of the above conditions are met, then 
	 * the method throws a {@link EdgeNeededException}.
	 * 
	 * Furthermore, if {@code to} already <em>happens before</em> {@code from},
	 * then a {@link CycleException} is thrown.
	 * 
	 * @throws EdgeNeededException see above.
	 * @throws CycleException see above. */
	public void addHb(Point to);
	
}
