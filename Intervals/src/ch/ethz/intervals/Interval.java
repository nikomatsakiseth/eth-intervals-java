package ch.ethz.intervals;

import static ch.ethz.intervals.EdgeList.NORMAL;
import ch.ethz.intervals.ThreadPool.Worker;
import ch.ethz.intervals.quals.Requires;
import ch.ethz.intervals.quals.Subinterval;

public abstract class Interval 
extends ThreadPool.WorkItem 
implements Dependency, Guard
{	
	private static final long serialVersionUID = 8105268455633202522L;
		
	public final Point start;
	public final Point end;
	final Line line;
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
		
		this.line = new Line(current, bound);		
		this.end = new Point(line, null, 2, null);
		this.start = new Point(line, this.end, 1, this);		
		current.addUnscheduled(this);
		
		bound.addWaitCount();
		if(current.start!= null) {
			current.start.addEdgeAfterOccurredWithoutException(start, NORMAL);		
			ExecutionLog.logNewInterval(current.start, start, end);
		} else
			ExecutionLog.logNewInterval(null, start, end);
		
		dep.addHbToNewInterval(this);		
	}
	
	Interval(Line line, Point start, Point end) {
		this.line = line;
		this.start = start;
		this.end = end;
	}
	
	/**
	 * Defines the behavior of the interval.  Must be
	 * overridden.  Executed by the scheduler when {@code this}
	 * is the current interval.  Do not invoke manually.
	 */
	@Requires(subinterval=@Subinterval(of="this"))
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
		Current cur = Current.push(this);
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
	
	/**
	 * Schedules {@code this} for execution.  You can also
	 * schedule all pending intervals using {@link Intervals#schedule()},
	 * or simply wait until the creating interval ends. 
	 * 
	 * @throws AlreadyScheduledException if already scheduled
	 */
	public final void schedule() throws AlreadyScheduledException {
		Current current = Current.get();
		current.schedule(this);
	}
	
	/**
	 * Returns the bounding point of this interval.
	 */
	public final Point bound() {
		return line.bound;
	}
	
	/**
	 * Returns {@link #end}, 
	 * thus ensuring that new intervals using
	 * {@code this} as their {@link Dependency} execute
	 * during {@code this}.
	 */
	@Override
	public final Point boundForNewInterval() {
		return end;
	}

	/**
	 * Adds an edge from {@link #start} to {@code inter.start},
	 * thus ensuring that new intervals using
	 * {@code this} as their {@link Dependency} execute
	 * during {@code this}.
	 */
	@Override
	public final void addHbToNewInterval(Interval inter) {
		start.addEdgeAndAdjust(inter.start, EdgeList.NORMAL);
	}

	/**
	 * True if the current interval is {@code this} or a subinterval of {@code this},
	 * or if the start of the current interval <em>happens after</em> {@code this.end}.
	 * 
	 * @see Guard#isReadable()
	 */
	@Override
	public final boolean isReadable() {
		Current current = Current.get();
		return current.line == line || (
				current.start != null && end.hb(current.start));
	}

	/**
	 * True if the current interval is {@code this} or a subinterval of {@code this}.
	 * 
	 * @see Guard#isWritable()
	 */
	@Override
	public final boolean isWritable() {
		Current current = Current.get();
		return current.line == line;
	}
	
	/**
	 * True if {@code this} will hold the lock {@code lock}
	 * when it executes.
	 */
	public final boolean holdsLock(Lock lock) {
		return line.holdsLock(lock);
	}
	
}
