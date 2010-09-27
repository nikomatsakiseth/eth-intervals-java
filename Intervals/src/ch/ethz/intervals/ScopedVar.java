package ch.ethz.intervals;

/**
 * The intervals equivalent of a thread local variable.
 * Creates a variable whose value is not scoped lexically
 * but rather dynamically.  Values set in a particular
 * interval can then be retrieved by any subinterval.
 * 
 * <p>Typically, the {@link #get()} method is used which 
 * simply returns the value associated with the closest
 * parent for which a value has been stored.
 * 
 * @param <T> the type of values stored in this value
 */
public interface ScopedVar<T> {
	
	/**
	 * The default value returned when no appropriate
	 * value exists. Typically {@code null}.
	 */
	T getDefaultValue();
	
	/** 
	 * Sets the value of this variable for all
	 * subintervals of {@code forInterval}, unless
	 * a more specific value is set for those
	 * subintervals.
	 * 
	 * Requires that the current interval is 
	 * {@code forInterval} or is an 
	 * inline subinterval of {@code forInterval}.
	 * 
	 * @param forInterval interval to associate the value with.  Must not be
	 * {@code null}.
	 * @param value value to be stored. Must not be {@code null}.
	 * @throws NullPointerException if {@code forInteval} or {@code value} 
	 * is {@code null}
	 * @throws IntervalException.MustBeCurrent if {@code forInterval} 
	 * is not the current interval or an inline subinterval of
	 * the current interval.
	 */
	void set(Interval forInterval, T value);
	
	/**
	 * Clears the entry for the given interval (if any).
	 * @param forInterval the interval whose entry should be cleared
	 * @throws NullPointerException if forInterval is null
	 * @throws IntervalException.MustBeCurrent if {@code forInterval}
	 * is not the current interval or an inline subinterval of
	 * the current interval.
	 */
	void clear(Interval forInterval);
	
	/**
	 * Reads the value of this variable for the current interval.
	 * 
	 * @return the value set for the current interval or the closest
	 * ancestor which has a value set.  If no value was set for any
	 * ancestor, returns {@link #getDefaultValue()}.
	 */
	T get();

	/**
	 * Like {@link #get()}, but returns the value appropriate for
	 * {@code forInterval}.  
	 * 
	 * @throws IntervalException.MustBeCurrent if {@code forInterval}
	 * is not the current interval or a subinterval of it.
	 */
	T get(Interval forInterval);

}
