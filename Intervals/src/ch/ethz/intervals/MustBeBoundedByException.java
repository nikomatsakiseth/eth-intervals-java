package ch.ethz.intervals;

import ch.ethz.intervals.impl.PointImpl;

public class MustBeBoundedByException extends IntervalException {
	private static final long serialVersionUID = -3545763123904421907L;
	
	public final PointImpl bound;
	public final PointImpl pointImpl;
	
	public MustBeBoundedByException(PointImpl bound, PointImpl pointImpl) {
		this.bound = bound;
		this.pointImpl = pointImpl;
	}
	
	public String toString() {
		return pointImpl + " must be bounded by " + bound;
	}
	
}
