package ch.ethz.intervals;

import static ch.ethz.intervals.EdgeList.NORMAL;
import ch.ethz.intervals.ThreadPool.Worker;

abstract class Interval 
extends ThreadPool.WorkItem 
implements Dependency
{	
	private static final long serialVersionUID = 8105268455633202522L;
	
	public final Point start;
	public final Point end;
	Interval nextUnscheduled; /** @see Current#unscheduled */
	
	public Interval(Dependency dep) {
		Point bound = dep.boundForNewInterval();
		Current current = Current.get();
		
		// Note: if this check passes, then no need to check for cycles 
		// for the path from current.start->bnd.  This is because we require
		// one of the following three conditions to be true, and in all three
		// cases a path current.start->bnd must already exist:
		// (1) Bound was created by us and is unscheduled.  Then a
		//     path current.start->bnd was already added.
		// (2) Bound is current.end.  current.start->current.end.
		// (3) A path exists from current.end -> bnd.  Same as (2).
		current.checkCanAddDep(bound);		
		
		this.end = new Point(current, bound, 2, null);
		this.start = new Point(current, this.end, 1, this);		
		current.addUnscheduled(this);
		
		bound.addWaitCount();
		if(current.start != null) 
			current.start.addEdgeAfterOccurredWithoutException(start, NORMAL);		
		ExecutionLog.logNewInterval(current.start, start, end);
		
		dep.addHbToNewInterval(this);		
	}

	/**
	 * Defines the behavior of the interval.  Must be
	 * overridden.  Executed by the scheduler when {@code this}
	 * is the current interval.  Do not invoke manually.
	 */
	protected abstract void run();
	
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
				try {
					run();
				} catch(Throwable t) {
					end.addPendingException(t);
				}

				cur.schedule();				
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
	
	public final void schedule() throws AlreadyScheduledException {
		Current current = Current.get();
		current.schedule(this);
	}
	
	@Override
	public Point boundForNewInterval() {
		return end;
	}

	@Override
	public final void addHbToNewInterval(Interval inter) {
		start.addEdgeAndAdjust(inter.start, EdgeList.NORMAL);
	}

}
