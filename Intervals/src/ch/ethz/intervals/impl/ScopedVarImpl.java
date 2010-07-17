package ch.ethz.intervals.impl;

import java.util.WeakHashMap;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.ScopedVar;

/** A rather naive implementation of scoped variables. */
public class ScopedVarImpl<T> implements ScopedVar<T> {
	
	private final T defaultValue;
	private final WeakHashMap<Interval, T> storedValues = new WeakHashMap<Interval, T>();
	
	public ScopedVarImpl(T defaultValue) {
		this.defaultValue = defaultValue;
	}

	@Override
	public T getDefaultValue() {
		return defaultValue;
	}

	@Override
	public void set(Interval forInterval, T value) {
		if(forInterval == null || value == null)
			throw new NullPointerException();
		
		Current current = Current.get();
		if(!forInterval.isInlineSubintervalOfOrEqualTo(current.inter))
			throw new IntervalException.MustBeCurrent(forInterval);
		
		synchronized(this) {
			storedValues.put(forInterval, value);
		}
	}
	
	private T getUnchecked(Interval forInterval) {
		if(forInterval == null) 
			return defaultValue;
		
		T value = storedValues.get(forInterval); 
		
		if(value != null)
			return value;
		
		return getUnchecked(forInterval.getParent());
	}

	@Override
	public synchronized T get() {
		return getUnchecked(Current.get().inter);
	}

	@Override
	public T get(Interval forInterval) {
		if(forInterval != null) {
			Current current = Current.get();
			if(current.inter == null) 
				throw new IntervalException.MustBeCurrent(forInterval);
			else if(!current.inter.isSubintervalOfOrEqualTo(forInterval))
				throw new IntervalException.MustBeCurrent(forInterval);
		}
		
		synchronized(this) {
			return getUnchecked(forInterval);
		}
	}

}
