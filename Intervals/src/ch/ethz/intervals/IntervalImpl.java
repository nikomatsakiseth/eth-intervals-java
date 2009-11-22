package ch.ethz.intervals;

import ch.ethz.intervals.ThreadPool.Worker;

class IntervalImpl<R> 
extends ThreadPool.WorkItem 
implements Interval<R> {
	
	private static final long serialVersionUID = 8105268455633202522L;
	
	protected Task<R> task;
	final PointImpl start;
	final PointImpl end;
	final IntervalFutureImpl<R> result;
	
	public IntervalImpl(Task<R> task, PointImpl start, PointImpl end) {
		this.task = task;
		this.start = start;
		this.end = end;
		this.result = new IntervalFutureImpl<R>(this.end);
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
	@SuppressWarnings("unchecked")
	void exec() {
		Intervals.currentInterval.set(this);
		
		Task<R> task = this.task;
		this.task = null; // for g.c., as it will not be needed again
		
		try {
			R returnValue = task.run(this);
			result.setResult(returnValue);
		} catch(Throwable t) {
			end.setPendingException(t);
		}
		
		end.arrive(1);
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

	@Override
	public IntervalFuture<R> future() {
		return result;
	}

}
