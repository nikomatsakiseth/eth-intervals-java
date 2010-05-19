package ch.ethz.intervals.impl;

import ch.ethz.intervals.AsyncInterval;
import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.Task;

public final class AsyncIntervalImpl 
extends IntervalImpl 
implements AsyncInterval {

	AsyncIntervalImpl(IntervalImpl parent, Task task) {
		super(parent, task);

		// attachedTo() must be called last, when the interval
		// is otherwise fully constructed.
		task.attachedTo(this);
	}

	@Override
	public void schedule() {
		Current current = Current.get();
		if(isUnscheduled(current)) {
			current.schedule(this);
		} else {
			throw new IntervalException.AlreadyScheduled(this);			
		}
	}

	@Override
	public void cancel(boolean unconditionally) {
		Current current = Current.get();
		if(isUnscheduled(current)) {
			cancel(start);
			current.schedule(this);
		} else if (unconditionally) {
			throw new IntervalException.AlreadyScheduled(this);			
		}
	}
	
	@Override
	public boolean isInline() {
		return false;
	}

	@Override
	IntervalImpl inlineBound() {
		return this;
	}

}
