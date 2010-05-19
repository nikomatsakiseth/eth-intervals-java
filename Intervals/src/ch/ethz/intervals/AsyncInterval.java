package ch.ethz.intervals;

public interface AsyncInterval extends Interval {
	
	/**
	 * Schedules this interval for eventual execution.
	 * The interval will execute once all of its happens-before
	 * dependencies are satisfied.  If this method is not
	 * invoked manually, the interval will be automatically scheduled when the
	 * current interval ends. 
	 * 
	 * @throws IntervalException.AlreadyScheduled under the
	 * same conditions as {@link Interval#cancel(boolean)}.
	 * 
	 * @see Interval#cancel(boolean)
	 */
	public void schedule();
	
}
