package ch.ethz.intervals.impl;

import ch.ethz.intervals.AsyncInterval;
import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.Task;

public class AsyncIntervalImpl 
extends IntervalImpl 
implements AsyncInterval {

	AsyncIntervalImpl(IntervalImpl parent, Task task) {
		super(parent, task);
		
		// Note: don't do anything here, as task.attachedTo()
		// has been called in super();
	}

	/**
	 * Schedules {@code this} for execution.  If this method is not
	 * invoked manually, the interval will be automatically scheduled when the
	 * current interval ends. 
	 * 
	 * @throws IntervalException.AlreadyScheduled if already scheduled
	 */
	public void schedule() throws IntervalException.AlreadyScheduled {
		Current current = Current.get();
		current.schedule(this);
	}

	@Override
	public boolean isSynchronous() {
		return false;
	}

}
