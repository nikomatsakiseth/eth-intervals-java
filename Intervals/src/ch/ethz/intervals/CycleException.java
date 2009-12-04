package ch.ethz.intervals;

/** Indicates that the user's action would have created a cycle in the
 *  scheduler graph. */
public class CycleException extends IntervalException {
	private static final long serialVersionUID = 2242825867497429060L;
	public final Point from, to;

	public CycleException(Point from, Point to) {
		super("Adding an edge from "+from+" to "+to+" would create a cycle");
		this.from = from;
		this.to = to;
	}
	
}
