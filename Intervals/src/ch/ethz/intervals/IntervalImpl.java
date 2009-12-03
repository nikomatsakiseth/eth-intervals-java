package ch.ethz.intervals;

import ch.ethz.intervals.ThreadPool.Worker;

class IntervalImpl 
extends ThreadPool.WorkItem 
implements Interval {
	
	private static final long serialVersionUID = 8105268455633202522L;
	
	protected Task task;
	final PointImpl start;
	final PointImpl end;
	IntervalImpl nextUnscheduled; /** @see Current#unscheduled */
	
	public IntervalImpl(Task task, PointImpl start, PointImpl end) {
		this.task = task;
		this.start = start;
		this.end = end;
	}
	
	/**
	 * The "main" method for this interval: invoked when we are scheduled.
	 * Simply invokes {@link #exec()}.
	 */
	@Override
	void exec(Worker worker) {
		exec();
	}

	/**
	 * Executes the interval's task and -- once it is finished -- signals 
	 * the end of the interval that it can occur (assuming all of its other
	 * dependencies are satisfied).
	 */
	void exec() {
		Current cur = Current.push(start, end);
		try {
			try {
				Task task = this.task;
				this.task = null; // for g.c., as it will not be needed again
				
				try {
					task.run(end);
				} catch(Throwable t) {
					end.addPendingException(t);
				}
				
				try {
					cur.schedule();					
				} catch(Throwable t) {
					end.addPendingException(t);
				}
				
				end.arrive(1);
			} catch(Throwable e) {
				e.printStackTrace(); // unexpected!
			}
		} finally { // I don't expect any exceptions, but...
			cur.pop();
		}
	}
	
	@Override
	public String toString() {
		return String.format("Interval(%s-%s)", start, end);
	}

	@Override
	public Point end() {
		return end;
	}

	@Override
	public Point start() {
		return start;
	}

	@Override
	public Point bound() {
		return end.bound;
	}

}
