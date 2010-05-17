package ch.ethz.intervals.impl;

import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.mirror.AsyncInterval;
import ch.ethz.intervals.mirror.Task;

public class AsyncIntervalImpl 
extends IntervalImpl 
implements AsyncInterval {

	AsyncIntervalImpl(IntervalImpl parent, Task task) {
		super(parent, task);
		
		// Note: don't do anything here, as task.attachedTo()
		// has been called in super();
	}

	/**
	 * Schedules {@code this} for execution.  You can also
	 * schedule all pending intervals using {@link Intervals#schedule()},
	 * or simply wait until the creating interval ends. 
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
