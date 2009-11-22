package ch.ethz.intervals;


public class NoEdgeException extends IntervalException {
	
	public final Point from;
	public final Point to;
	
	public NoEdgeException(Point from, Point to) {
		this.from = from;
		this.to = to;
	}
	
}
