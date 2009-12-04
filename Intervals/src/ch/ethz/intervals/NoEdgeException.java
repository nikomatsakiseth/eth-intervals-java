package ch.ethz.intervals;


public class NoEdgeException extends IntervalException {
	private static final long serialVersionUID = 2919767931668050187L;
	
	public final Point from;
	public final Point to;
	
	public NoEdgeException(Point from, Point to) {
		this.from = from;
		this.to = to;
	}
	
}
