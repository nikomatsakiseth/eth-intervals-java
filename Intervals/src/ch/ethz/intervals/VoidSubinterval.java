package ch.ethz.intervals;

public abstract class VoidSubinterval {
	/** @see SubintervalTask#locks() */
	public Lock[] locks() {
		return null;
	}

	/** @see SubintervalTask#run(Interval) */
	public abstract void run(Interval subinterval);
}
