package ch.ethz.intervals;

/**
 * @see Intervals#blockingSetupInterval(SetupTask)
 */
public interface SetupTask<R> {	
	public R setup(Interval<R> current, Interval<Void> worker);
}
