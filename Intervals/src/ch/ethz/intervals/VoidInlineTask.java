package ch.ethz.intervals;

public abstract class VoidInlineTask {
	/** @see InlineTask#locks() */
	public Lock[] locks() {
		return null;
	}

	/** @see InlineTask#run(Interval) */
	public abstract void run(Interval subinterval);
}
