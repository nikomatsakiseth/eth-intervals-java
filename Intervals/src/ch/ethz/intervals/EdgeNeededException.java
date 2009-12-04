package ch.ethz.intervals;

/** Indicates that the <em>happens before</em> relation
 *  was insufficient for the operation to be safe.  */
public class EdgeNeededException extends IntervalException {
	private static final long serialVersionUID = 2919767931668050187L;
	
	public final Point from;
	public final Point to;
	
	public EdgeNeededException(Point from, Point to) {
		this.from = from;
		this.to = to;
	}
	
}
