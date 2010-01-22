package ch.ethz.intervals;

import ch.ethz.intervals.ThreadPool.Worker;
import ch.ethz.intervals.guard.Guard;
import ch.ethz.intervals.mirror.IntervalMirror;
import ch.ethz.intervals.mirror.LockMirror;
import ch.ethz.intervals.mirror.PointMirror;
import ch.ethz.intervals.quals.Requires;
import ch.ethz.intervals.quals.Subinterval;

public abstract class Interval 
extends ThreadPool.WorkItem 
implements Dependency, Guard, IntervalMirror
{
	static final int SYNCHRONOUS_FLAG = 1;
	
	private static final ChunkList<Interval> runMethodTerminated = ChunkList.empty();
	
	private static final long serialVersionUID = 8105268455633202522L;
		
	public final Interval parent;
	public final Point start;
	public final Point end;
	
	private final String name;
	
	/** If we are unscheduled, who are we "unscheduled" by? */
	private Current unscheduled;
	
	/** @see Current#unscheduled */
	Interval nextUnscheduled;

	/** Linked list of locks to acquire before executing.  Modified only when
	 *  synchronized, but once start point occurs can now longer be modified,
	 *  so used without synchronization in methods that must happen after start. 
	 *  <b>Warning:</b> Stored in THE REVERSE ORDER of how they should be acquired! */
	private LockList revLocks;
	
	/** Any child intervals created before run method finishes execution.
	 *  Once run method completes, set to {@link #runMethodTerminated} and never
	 *  changed. Modified only under lock! */
	private ChunkList<Interval> pendingChildIntervals = null;
	
	public Interval(Dependency dep) {
		this(dep, null);
	}
	
	public Interval(Dependency dep, String name) {
		Interval parent = dep.parentForNewInterval();
		Current current = Current.get();
		
		// If parent == null, then direct child of root interval:
		Point parentEnd = Intervals.end(parent);
		
		// Note: if this check passes, then no need to check for cycles 
		// for the path from current.start->bnd.  This is because we require
		// one of the following three conditions to be true, and in all three
		// cases a path current.start->bnd must already exist:
		// (1) Bound was created by us and is unscheduled.  Then a
		//     path current.start->bnd was already added.
		// (2) Bound is current.end.  current.start->current.end.
		// (3) A path exists from current.end -> bnd.  Same as (2).
		if(parentEnd != null) current.checkCanAddDep(parentEnd);	
		
		this.name = name;
		this.parent = parent;
		end = new Point(name, Point.FLAG_END, parentEnd, 2, this);
		start = new Point(name, Point.NO_FLAGS, end, 2, this);		
		
		unscheduled = current;
		current.addUnscheduled(this);
		int expFromParent = (parent != null ? parent.addChildInterval(this) : 0);
		if(expFromParent == 0) start.addWaitCountUnsync(-1);
				
		ExecutionLog.logNewInterval(null, start, end);
		
		dep.addHbToNewInterval(this);		
	}
	
	Interval(String name, Interval parent, int pntFlags, int startWaitCount, int endWaitCount) {
		this.name = name;
		this.parent = parent;		
		this.end = new Point(name, pntFlags | Point.FLAG_END, Intervals.end(parent), endWaitCount, this);
		this.start = new Point(name, pntFlags, end, startWaitCount, this);
		
		if(end.bound != null)
			end.bound.addWaitCount();
	}
	
	int addChildInterval(Interval inter) {
		end.addWaitCount();

		synchronized(this) {
			if(pendingChildIntervals == runMethodTerminated) {
				// Run method already finished.  New children should just start.
				return 0;
			} else {
				// Run method not yet finished.  New children must wait.
				pendingChildIntervals = ChunkList.add(pendingChildIntervals, inter, ChunkList.NO_FLAGS);
				return 1;
			}
		}
	}
	
	void setRevLocksUnsync(LockList locks) {
		revLocks = locks;
	}
	
	/** Note: Safety checks apply!  See {@link Current#checkCanAddDep(Point)} */ 
	void addExclusiveLock(Lock lock, Guard guard) {
		LockList list = new LockList(this, lock, guard, null);
		synchronized(this) {
			list.next = revLocks;			
			setRevLocksUnsync(list);
		}
	}
	
	synchronized LockList revLocksSync() {
		return revLocks;
	}
	
	/** Invoked when the wait count of {@code point} reaches zero. 
	 *  For the start point, begins acquiring locks, and invokes
	 *  {@link Point#occur()} when complete.  For the end point,
	 *  simply invokes {@link Point#occur()}. 
	 */
	void didReachWaitCountZero(Point point, boolean hasPendingExceptions) {
		if(point == start) {
			acquireNextUnacquiredLock(hasPendingExceptions);
		} else {
			assert point == end;
			end.occur();
		}
	}
	
	/** So long as {@code hasPendingExceptions} is false, 
	 *  acquirse any unacquired locks, invoking {@link Point#occur()}
	 *  when complete.  If some lock cannot be immediately acquired,
	 *  then it returns and waits for a callback when the lock has
	 *  been acquired. */
	void acquireNextUnacquiredLock(boolean hasPendingExceptions) {		
		if(!hasPendingExceptions) {
			// Find the last lock which has not yet been acquired.
			// It is safe to use unsynchronized reads/writes here, 
			// because any modification to this list must have been 
			// during some interval which HB the start point.
			LockList ll1 = null, ll2 = revLocks;
			while(ll2 != null) {
				if(ll2.acquiredLock != null)
					break;
				ll1 = ll2;
				ll2 = ll2.next;
			}
			if(ll1 != null) {
				acquireLock(ll1);
				return;
			}
		}
		start.occur();
		return;
	}
	
	void didAcquireLock(LockList ll) {
		assert ll.acquiredLock != null;
		
		// Check that acquiring this lock did not
		// create a data race:		
		Throwable err = (ll.guard == null ? null : ll.guard.checkLockable(this, ll.lock));
		
		if(err != null) {
			start.addPendingException(err);		
			acquireNextUnacquiredLock(true);
		} else
			acquireNextUnacquiredLock(false);
	}

	private int acquireLock(LockList thisLockList) {
		// First check whether we are recursively acquiring this lock:
		for(Interval ancestor = parent; ancestor != null; ancestor = ancestor.parent) {
			for(LockList ancLockList = ancestor.revLocks; ancLockList != null; ancLockList = ancLockList.next)
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
			// After start point occurs, if no exceptions then run user task.
			if(!hasPendingExceptions) Intervals.POOL.submit(this);
			else finishSequentialPortion(null);
		} else {
			// After end point occurs, release any locks we acquired.
			assert pnt == end;
			for(LockList lock = revLocks; lock != null; lock = lock.next)
				// If there were pending exceptions, we may not have acquired any locks:
				if(lock.acquiredLock != null)
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
				cur.schedule();								
				finishSequentialPortion(null);
			} catch(Throwable e) {
				e.printStackTrace(); // unexpected!
			}
		} finally { // I don't expect any exceptions, but...
			cur.pop();
		}
	}
	
	private void finishSequentialPortion(final LittleLinkedList<Throwable> extraErrors) {
		ChunkList<Interval> pending;
		synchronized(this) {
			pending = pendingChildIntervals;
			this.pendingChildIntervals = runMethodTerminated;
		}
		
		if(pending != null) {
			new ChunkList.Iterator<Interval>(pending) {
				@Override public void doForEach(Interval child, int flags) {
					if(extraErrors != null)
						for(LittleLinkedList<Throwable> extraError = extraErrors; extraError != null; extraError = extraError.next)
							child.start.addPendingException(extraError.value);
					child.start.arrive(1);
				}
			};
		}

		end.arrive(1);
	}

	@Override
	public String toString() {
		if(name != null)
			return name;
		return String.format("Interval(%s-%s)", start, end);
	}
	
	/**
	 * Schedules {@code this} for execution.  You can also
	 * schedule all pending intervals using {@link Intervals#schedule()},
	 * or simply wait until the creating interval ends. 
	 * 
	 * @throws IntervalException.AlreadyScheduled if already scheduled
	 */
	public final void schedule() throws IntervalException.AlreadyScheduled {
		Current current = Current.get();
		current.schedule(this);
	}
	
	/**
	 * Returns the bounding point of this interval.
	 */
	public final Point bound() {
		return end.bound;
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
		start.addEdgeAndAdjust(inter.start, ChunkList.NORMAL);
	}
	
	private boolean isWritableBy(IntervalMirror inter) {
		while(inter != this && inter.isSynchronous())
			inter = inter.parent();
		return (inter == this);
	}
	
	private boolean isReadableBy(PointMirror mr, IntervalMirror inter) {
		return (isWritableBy(inter) || (mr != null && end.hbeq(mr)));
	}
	
	@Override
	public IntervalException checkLockable(IntervalMirror interval, LockMirror lock) {
		return new IntervalException.CannotBeLockedBy(this, lock);
	}

	@Override
	public IntervalException checkReadable(PointMirror mr, IntervalMirror inter) {
		if(!isReadableBy(mr, inter))
			return new IntervalException.MustHappenBefore(end, mr);
		return null;
	}

	@Override
	public IntervalException checkWritable(PointMirror mr, IntervalMirror inter) {
		if(!isWritableBy(inter))
			return new IntervalException.NotSubinterval(inter, this);
		return null;
	}
	
	/**
	 * True if {@code this} will hold the lock {@code lock}
	 * when it executes, or it is a blocking subinterval of
	 * someone who will.
	 */
	@Override public final boolean locks(LockMirror lock) {
		if(holdsLockItself(lock))
			return true;
		
		if(isSynchronous() && parent != null)
			return parent.locks(lock);
		
		return false;
	}
	
	/**
	 * True if {@code this} will hold the lock {@code lock}
	 * when it executes.
	 */
	final boolean holdsLockItself(LockMirror lock) {
		for(LockList ll = revLocksSync(); ll != null; ll = ll.next)
			if(ll.lock == lock)
				return true;
		return false;
	}
	
	@Override public PointMirror start() {
		return start;
	}
	
	@Override public PointMirror end() {
		return end;
	}
	
	@Override public void addLock(LockMirror lock, Guard guard) {
		if(lock instanceof Lock) {
			Intervals.addExclusiveLock(this, (Lock) lock, guard);
		} else {
			throw new UnsupportedOperationException("Must be subtype of Lock");
		}
	}

	@Override
	public boolean isSynchronous() {
		return start.isSynchronous();
	}

	@Override
	public Interval parent() {
		return parent;
	}

	/**
	 * Returns true if this interval is due to be scheduled by
	 * {@cure current}.
	 */
	boolean isUnscheduled(Current current) {
		// No need to worry about race conditions: if our field == current,
		// then current is the only thread that will ever change it.
		// Otherwise, going to return false anyhow.
		return (unscheduled == current);
	}

	void clearUnscheduled() {
		// Only invoked by our scheduler:
		unscheduled = null;
	}
	
}
