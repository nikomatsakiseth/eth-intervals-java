package ch.ethz.intervals;

public abstract class VoidInlineTask {
	/** @see InlineTask#init(Interval) */
	public void init(Interval subinterval) {}

	/** @see InlineTask#run(Interval) */
	public abstract void run(Interval subinterval);
}
