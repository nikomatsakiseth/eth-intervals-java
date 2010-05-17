package ch.ethz.intervals;

import ch.ethz.intervals.impl.PointImpl;

/** Indicates that the <em>happens before</em> relation
 *  was insufficient for the operation to be safe.  */
public class EdgeNeededException extends IntervalException {
	private static final long serialVersionUID = 2919767931668050187L;
	
	public final PointImpl from;
	public final PointImpl to;
	
	public EdgeNeededException(PointImpl from, PointImpl to) {
		this.from = from;
		this.to = to;
	}
	
}
