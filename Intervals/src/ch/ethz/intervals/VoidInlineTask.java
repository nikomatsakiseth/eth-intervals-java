package ch.ethz.intervals;

import ch.ethz.intervals.quals.Is;

@Subinterval
public abstract class VoidInlineTask {
	/** @see InlineTask#init(Interval) */
	public void init(@Is("Subinterval") Interval subinterval) {}

	/** @see InlineTask#run(Interval) */
	public abstract void run(@Is("Subinterval") Interval subinterval);
}
