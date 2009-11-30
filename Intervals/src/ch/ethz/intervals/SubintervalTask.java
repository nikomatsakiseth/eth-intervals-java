package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.intervalWithBound;

/**
 * Wraps one task in a subinterval.  Useful when a task could
 * create siblings and you want to wait for those too.
 */
public class SubintervalTask extends AbstractTask {
	
	public final Task subtask;
	
	public SubintervalTask(Task t) {
		this.subtask = t;
	}

	@Override
	public final void run(Point parentEnd) {
		intervalWithBound(parentEnd, subtask);
	}
	
	public String toString() {
		return "Subinterval("+subtask+")";
	}

}
