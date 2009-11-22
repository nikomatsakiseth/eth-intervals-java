package ch.ethz.intervals;

public class CycleException extends IntervalException {
	public final Point from, to;

	public CycleException(Point from, Point to) {
		super("Adding an edge from "+from+" to "+to+" would create a cycle");
		this.from = from;
		this.to = to;
	}
	
}
