package ch.ethz.intervals;

public interface InlineInterval extends Interval {
	
	/** 
	 * Executes the inline interval and does not return until
	 * the inline interval has completed.  This method
	 * must be invoked <b>exactly once</b> on all inline
	 * interval objects, and it must be invoked by the inline
	 * interval's parent! If it is not invoked by the time that
	 * the parent of the inline interval completes, an 
	 * {@link IntervalException.InlineIntervalNeverExecuted}
	 * exception will be thrown.  If it is invoked twice,
	 * then a {@link IntervalException.AlreadyExecuted}
	 * exception will be thrown. */
	public void execute();
	
}
