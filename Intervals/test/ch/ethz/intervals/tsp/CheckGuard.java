package ch.ethz.intervals.tsp;

import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.guard.Guard;

public class CheckGuard<D> {
	private D data;
	private Guard guard;
	
	public CheckGuard(D initial, Guard guard) {
		this.data = initial;
		this.guard = guard;
	}
	
	public D get() {
		Intervals.checkReadable(guard);
		return data;
	}
	
	public D set(D newValue) {
		Intervals.checkWritable(guard);
		D oldValue = data;
		data = newValue;
		return oldValue;
	}
	
}
