package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.intervalWithBound;

/**
 * Wraps one task in a subinterval.  Useful when a task could
 * create siblings and you want to wait for those too.
 */
public class SubintervalWrapper implements Task<Void> {
	
	final Task<?> task;
	
	public SubintervalWrapper(Task<?> t) {
		this.task = t;
	}

	@Override
	public Void run(Interval<Void> current) {
		intervalWithBound(current.end()).schedule(task);
		return null;
	}
	
	public String toString() {
		return "Subinterval("+task+")";
	}

}
