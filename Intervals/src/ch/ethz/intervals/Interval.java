package ch.ethz.intervals;

import static ch.ethz.intervals.EdgeList.NORMAL;
import static ch.ethz.intervals.Point.FLAG_ACQUIRE_LOCKS;
import static ch.ethz.intervals.Point.NO_POINT_FLAGS;
import ch.ethz.intervals.ThreadPool.Worker;
import ch.ethz.intervals.quals.Requires;
import ch.ethz.intervals.quals.Subinterval;

public abstract class Interval 
extends ThreadPool.WorkItem 
implements Dependency, Guard
{	
	private static final long serialVersionUID = 8105268455633202522L;
		
	public final Interval parent;
	public final Point start;
	public final Point end;
	
	final Line line;
	
	Interval nextUnscheduled; /** @see Current#unscheduled */

	private LockList locks;
	private LittleLinkedList<Interval> pendingChildIntervals = null;
	
	public Interval(Dependency dep) {
		Interval parent = dep.parentForNewInterval();
		Current current = Current.get();
		
		// If parent == null, then direct child of root interval:
		Point parentStart, parentEnd;
		if(parent != null) {
			parentStart = parent.start;
			parentEnd = parent.end;
		} else {
			parentStart = null;
			parentEnd = null;
		}

		
		// Note: if this check passes, then no need to check for cycles 
		// for the path from current.start->bnd.  This is because we require
		// one of the following three conditions to be true, and in all three
		// cases a path current.start->bnd must already exist:
		// (1) Bound was created by us and is unscheduled.  Then a
		//     path current.start->bnd was already added.
		// (2) Bound is current.end.  current.start->current.end.
		// (3) A path exists from current.end -> bnd.  Same as (2).
		if(parentEnd != null) current.checkCanAddDep(parentEnd);	
		
		this.parent = parent;
		line = new Line(current, parentEnd);		
		end = new Point(line, null, null, NO_POINT_FLAGS, 2, null);
		start = new Point(line, end, end, FLAG_ACQUIRE_LOCKS, 2, this);		
		
		current.addUnscheduled(this);
		int expFromParent = (parent != null ? parent.addChildInterval(this) : 0);
		if(expFromParent == 0) start.addWaitCountUnsync(-1);
				
		if(current.mr != parentStart) {
			current.mr.addEdgeAfterOccurredWithoutException(start, NORMAL);		
			ExecutionLog.logNewInterval(current.mr, start, end);
		} else
			ExecutionLog.logNewInterval(null, start, end);
		
		dep.addHbToNewInterval(this);		
	}
	
	Interval(Interval parent, Point start, Point end) {
		assert start != null && end != null && start.line == end.line;
		this.parent = parent;
		this.line = start.line;
		this.start = start;
		this.end = end;
	}
	
	int addChildInterval(Interval inter) {
		end.addWaitCount();
		
		synchronized(start) {
			if(start.didOccur())
				return 0;
			else {
				pendingChildIntervals = new LittleLinkedList<Interval>(inter, pendingChildIntervals);
				return 1;
			}
		}
	}
	
	/** Note: Safety checks apply!  See {@link Current#checkCanAddDep(Point)} */ 
	void addLock(Lock lock, boolean exclusive) {
		LockList list = new LockList(lock, exclusive, null);
		synchronized(this) {
			list.next = locks;
			locks = list;
		}
	}
	
	synchronized LockList locksSync() {
		return locks;
	}
	
	LockList locksUnsync() {
		return locks;
	}
	
	/**
	 * Invoked by {@link #start} when all HB dependencies are resolved.
	 * Returns true if the caller should wait for locks to be acquired,
	 * or false if they are all acquired upon return.
	 */
	boolean acquirePendingLocks() {		
		// It is safe to use an unsynchronized read here, because any modification
		// to this list must have been during some interval which HB the start point.
		LockList lock = locksUnsync();
		if(lock != null) {
			start.addWaitCountUnsync(1); // sync not needed here, all pred. arrived
			for(; lock != null; lock = lock.next)
				acquireLock(lock.exclusive, lock.lock);
			start.arrive(1); // now we may have pred. again, need sync
			return true;
		} else
			return false;
	}

	private void acquireLock(boolean exclusive, Lock lock) {
		assert exclusive;
		
		// First check whether we are recursively acquiring this lock:
		for(Interval ancestor = parent; ancestor != null; ancestor = ancestor.parent) {
			for(LockList lockList = ancestor.locksUnsync(); lockList != null; lockList = lockList.next)
				if(lockList.lock == lock) {
					lockList.addExclusive(start, end);
					return;
				}
		}
		
		// If not:
		lock.addExclusive(start, end);							
	}

	/**
	 * Defines the behavior of the interval.  Must be
	 * overridden.  Executed by the scheduler when {@code this}
	 * is the current interval.  Do not invoke manually.
	 */
	@Requires(subinterval=@Subinterval(of="this"))
	protected abstract void run();
	
	/** 
	 * Invoked when the start point for this interval occurs.  If no errors
	 * are pending, schedules {@code this} for execution.  Otherwise, just
	 * signals {@link #end} that we have completed (the {@code #exec(Worker)}
	 * method should only be run if there were no pending exceptions).  
	 *  
	 * @param exceptionsPending true if the start point has exceptions pending */
	final void fork(boolean exceptionsPending) {
		if(!exceptionsPending) {
			Intervals.POOL.submit(this);
		} else {
			end.arrive(1);
		}
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
	final void exec() {
		for(LittleLinkedList<Interval> pending = pendingChildIntervals; pending != null; pending = pending.next)
			pending.value.start.arrive(1);
		pendingChildIntervals = null;
		
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
	public final Interval parentForNewInterval() {
		return this;
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
		return current.inter.line == line || (
				current.mr != null && end.hb(current.mr));
	}

	/**
	 * True if the current interval is {@code this} or a subinterval of {@code this}.
	 * 
	 * @see Guard#isWritable()
	 */
	@Override
	public final boolean isWritable() {
		Current current = Current.get();
		return current.inter.line == line;
	}
	
	/**
	 * True if {@code this} will hold the lock {@code lock}
	 * when it executes.
	 */
	public final boolean holdsLock(Lock lock) {
		for(LockList ll = locksSync(); ll != null; ll = ll.next)
			if(ll.lock == lock)
				return true;
		return false;
	}

}
