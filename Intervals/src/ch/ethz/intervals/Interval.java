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
		
	public final Interval parent;
	public final Point start;
	public final Point end;
	
	/** @see Current#unscheduled */
	Interval nextUnscheduled;

	/** Linked list of locks to acquire before executing.  Modified only when
	 *  synchronized, but once start point occurs can now longer be modified,
	 *  so used without synchronization in methods that must happen after start. */
	private LockList locks;
	
	/** Any child intervals created before {@link #start} has occurred are
	 *  added to this list.  Must lock {@link #start} when modifying to 
	 *  be sure that {@link #start} does not occur. */
	private LittleLinkedList<Interval> pendingChildIntervals = null;
	
	private String pntName(String prefix, String suffix) {
		if(prefix == null) return null;
		return prefix + suffix;
	}
	
	public Interval(Dependency dep) {
		this(dep, null);
	}
	
	public Interval(Dependency dep, String name) {
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
		Line line = new Line(current, parentEnd);		
		end = new Point(name, line, null, null, 2, this);
		start = new Point(name, line, end, end, 2, this);		
		
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
	
	Interval(String name, Interval parent, Line line, int startWaitCount, int endWaitCount, Point nextEpoch) {
		assert line != null;
		this.parent = parent;		
		this.end = new Point(name, line, null, nextEpoch, endWaitCount, this);
		this.start = new Point(name, line, end, end, startWaitCount, this);
		
		if(nextEpoch != null)
			nextEpoch.addWaitCount();
	}
	
	Line line() {
		return end.line;
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
	void addExclusiveLock(Lock lock) {
		LockList list = new LockList(this, lock, null);
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
	
	/** Invoked when the wait count of {@code point} reaches zero. 
	 *  If it returns {@code true}, then the point should occur.
	 *  Otherwise, if it returns false, new dependencies have been added and 
	 *  the point should not occur yet.  */
	boolean willOccur(Point point, boolean hasPendingExceptions) {
		if(point == start && !hasPendingExceptions) {
			// It is safe to use an unsynchronized read here, because any modification
			// to this list must have been during some interval which HB the start point.
			LockList lockList = locksUnsync();
			if(lockList != null && lockList.acquiredLock == null) {
				start.addWaitCountUnsync(1); // sync not needed here, all pred. arrived
				for(; lockList != null; lockList = lockList.next)
					acquireLock(lockList);
				start.arrive(1); // now we may have pred. again, need sync
				return false;
			} 
		}
		
		return true;
	}

	private int acquireLock(LockList thisLockList) {
		// First check whether we are recursively acquiring this lock:
		for(Interval ancestor = parent; ancestor != null; ancestor = ancestor.parent) {
			for(LockList ancLockList = ancestor.locksUnsync(); ancLockList != null; ancLockList = ancLockList.next)
				if(ancLockList.lock == thisLockList.lock) {
					return ancLockList.tryAndEnqueue(thisLockList);
				}
		}
		
		// If not:
		return thisLockList.lock.tryAndEnqueue(thisLockList);							
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
	 * @param hasPendingExceptions true if the start point has exceptions pending */
	final void didOccur(Point pnt, boolean hasPendingExceptions) {
		assert pnt.didOccur();
		if(pnt == start) {
			// First awaken any children.
			for(LittleLinkedList<Interval> pending = pendingChildIntervals; pending != null; pending = pending.next)
				pending.value.start.arrive(1);
			pendingChildIntervals = null;
			
			// After start point occurs, if no exceptions then run user task.
			if(!hasPendingExceptions) Intervals.POOL.submit(this);
			else end.arrive(1); // otherwise just signal the end point
		} else {
			// After end point occurs, release any locks we acquired.
			assert pnt == end;
			for(LockList lock = locksUnsync(); lock != null; lock = lock.next)
				lock.unlockAcquiredLock();
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
		Current cur = Current.push(this);
		try {
			try {
				try {
					run();
				} catch(Throwable t) {
					end.addPendingException(t);
				}

				// n.b.: This does not release the locks we acquired.  It releases the *recursive*
				// versions of those locks, so that our subintervals can now acquire them!
				// The locks themselves are released to non-subintervals in didOccur() after end occurs. 
				for(LockList lockList = this.locks; lockList != null; lockList = lockList.next)
					lockList.unlockThis();

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
		return end.line.bound;
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
		return current.inter.line() == line() || (
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
		return current.inter.line() == line();
	}
	
	/**
	 * True if {@code this} will hold the lock {@code lock}
	 * when it executes.
	 */
	public final boolean holdsLock(Lock lock) {
		for(LockList ll = locksSync(); ll != null; ll = ll.next)
			if(ll.lock == lock)
				return true;
		
		if(parent != null && parent.line() == line())
			return parent.holdsLock(lock);
		
		return false;
	}

}
