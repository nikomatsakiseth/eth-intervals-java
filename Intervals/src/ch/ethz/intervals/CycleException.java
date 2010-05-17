package ch.ethz.intervals;

import ch.ethz.intervals.impl.PointImpl;

/** Indicates that the user's action would have created a cycle in the
 *  scheduler graph. */
public class CycleException extends IntervalException {
	private static final long serialVersionUID = 2242825867497429060L;
	public final PointImpl from, to;

	public CycleException(PointImpl from, PointImpl to) {
		super("Adding an edge from "+from+" to "+to+" would create a cycle");
		this.from = from;
		this.to = to;
	}
	
}
