package ch.ethz.intervals;

public class MustBeBoundedByException extends IntervalException {
	private static final long serialVersionUID = -3545763123904421907L;
	
	public final Point bound;
	public final Point point;
	
	public MustBeBoundedByException(Point bound, Point point) {
		this.bound = bound;
		this.point = point;
	}
	
	public String toString() {
		return point + " must be bounded by " + bound;
	}
	
}
