package ch.ethz.intervals.impl;

import java.util.Set;

import pcollections.Empty;
import pcollections.HashTreePSet;
import pcollections.PSet;
import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.RethrownException;
import ch.ethz.intervals.guard.Guard;
import ch.ethz.intervals.impl.ThreadPool.Worker;
import ch.ethz.intervals.mirror.AsyncInterval;
import ch.ethz.intervals.mirror.InlineInterval;
import ch.ethz.intervals.mirror.Interval;
import ch.ethz.intervals.mirror.Lock;
import ch.ethz.intervals.mirror.Point;
import ch.ethz.intervals.mirror.Task;
import ch.ethz.intervals.util.ChunkList;

public abstract class IntervalImpl 
extends ThreadPool.WorkItem 
implements Guard, Interval
{	
	// =====================================================================================
	// Public interface (and some private supporting functions):
	
	@Override
	public String toString() {
		if(name != null)
			return name;
		return String.format("Interval(%s-%s)", start, end);
	}
	
	/**
	 * Returns the bounding point of this interval.
	 */
	public final PointImpl bound() {
		return end.bound;
	}
	
	private boolean isWritableBy(Interval inter) {
		while(inter != this && inter.isSynchronous())
			inter = inter.getParent();
		return (inter == this);
	}
	
	private boolean isReadableBy(Point mr, Interval inter) {
		return (isWritableBy(inter) || (mr != null && end.hbeq(mr)));
	}
	
	@Override
	public IntervalException checkLockable(Interval interval, Lock lock) {
		return new IntervalException.CannotBeLockedBy(this, lock);
	}

	@Override
	public IntervalException checkReadable(Point mr, Interval inter) {
		if(!isReadableBy(mr, inter))
			return new IntervalException.MustHappenBefore(end, mr);
		return null;
	}

	@Override
	public IntervalException checkWritable(Point mr, Interval inter) {
		if(!isWritableBy(inter))
			return new IntervalException.NotSubinterval(inter, this);
		return null;
	}
	
	/**
	 * True if {@code this} will hold the lock {@code lock}
	 * when it executes, or it is a blocking subinterval of
	 * someone who will.
	 */
	@Override public final boolean locks(Lock lock) {
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
	final boolean holdsLockItself(Lock lock) {
		for(LockList ll = revLocksSync(); ll != null; ll = ll.next)
			if(ll.lockImpl == lock)
				return true;
		return false;
	}
	
	@Override public Point getStart() {
		return start;
	}
	
	@Override public Point getEnd() {
		return end;
	}
	
	@Override public void addLock(Lock lock) {
		addLock(lock, null);
	}
	
	@Override public void addLock(Lock _lock, Guard guard) {
		LockImpl lock = (LockImpl) _lock;
		
		Current current = Current.get();
		current.checkCanAddDep(start);
		addExclusiveLock(lock, guard);
	}

	@Override
	public boolean isSynchronous() {
		return start.isSynchronous();
	}

	@Override
	public IntervalImpl getParent() {
		return parent;
	}

	@Override
	public AsyncInterval newAsyncChild(Task task) {
		return new AsyncIntervalImpl(this, task);
	}

	@Override
	public InlineInterval newInlineChild(Task task) {
		// Warning: there is code in FactoryImpl that
		// creates new InlineIntervalImpl's directly, 
		// bypassing these safety checks (because it
		// knows they will pass).

		if(Intervals.SAFETY_CHECKS && Current.get().inter != this)
			throw new IntervalException.MustBeCurrent(this);
		return new InlineIntervalImpl(this, task);
	}

	// =====================================================================================
	// Package or private interface:
	
	final IntervalImpl parent;
	final PointImpl start;
	final PointImpl end;
	final Task task;
	
	/* State diagram for intervals:
	 * 
	 *  WAIT --------(1)-> LOCK -> RUN --> PAR --------(2)----> END
	 *   |                  |       |        |                  ^
	 *   |                  |       |        +--(error,2)--+    | (no errors)
	 *   |                  |       |                      |    | 
	 * (cancel)             +----(error)-> CATCH_PAR --(2)-+--> CATCH
	 *   |                                                      | 
	 *   |                                                      | (errors)
	 *   v                                                      v
	 *  CANCEL_WAIT -(1)-----------------> CANCEL_PAR -(2)----> ERROR_END
	 *  
	 * (1) Wait count of start reached zero
	 * (2) Wait count of end reached zero
	 * 
	 * Start point occurs upon exiting LOCK or CANCEL_WAIT state.
	 * End point occurs upon entering END or ERROR_END state.
	 */
	private static final int PERMITS_CHILDREN = 1;
	private static final int WILL_SIGNAL_CHILDREN = 2;
	private static final int CHILDREN_CANCELLED = 4;
	enum State {
		// Normal, non-error states:
		WAIT(PERMITS_CHILDREN | WILL_SIGNAL_CHILDREN),
		LOCK(PERMITS_CHILDREN | WILL_SIGNAL_CHILDREN),
		RUN(PERMITS_CHILDREN | WILL_SIGNAL_CHILDREN),
		PAR(PERMITS_CHILDREN),
		
		// Errors occuring within this interval:
		CATCH_PAR(PERMITS_CHILDREN | CHILDREN_CANCELLED),
		CATCH(0),
		
		// Errors occuring within a parent or pred:
		CANCEL_WAIT(PERMITS_CHILDREN | WILL_SIGNAL_CHILDREN | CHILDREN_CANCELLED),
		CANCEL_PAR(PERMITS_CHILDREN | CHILDREN_CANCELLED),

		// End states:
		END(0),          /** Ended without errors. */
		ERROR_END(0);    /** Ended with uncaught errors or cancelled by errors in parent/pred. */
		
		/** If false, creating a new child while in this state yields a 
		 *  {@link IntervalException.ParPhaseCompleted} error. */
		public final boolean permitsNewChildren;
		
		/** If true, a child added during this state will later be signalled
		 *  when it is time to start.  If false, they may start immediately. 
		 *  Always false if {@link #permitsNewChildren} is false. */
		public final boolean willSignalChild;
		
		/** True if children creates in this state should be CANCELLED */
		public final boolean childrenCancelled;

		private State(int flags) {			
			this.permitsNewChildren = (flags & PERMITS_CHILDREN) != 0;
			this.willSignalChild = (flags & WILL_SIGNAL_CHILDREN) != 0;
			this.childrenCancelled = (flags & CHILDREN_CANCELLED) != 0;
		}
	}
	
	private static final long serialVersionUID = 8105268455633202522L;
		
	/** User-provided name for the interval, or null */
	private final String name;
	
	/** If we are unscheduled, who are we "unscheduled" by? */
	private Current unscheduled;
	
	/** @see Current#unscheduled */
	IntervalImpl nextUnscheduled;
	
	/** Current state of this interval.  
	 * 
	 *  Any change to this variable must HAPPEN AFTER the predecessor state.
	 * 
	 *  Locking policy:
	 *  <ul>
	 *  <li> The only time that the state is externally affected is when pred. and parents invoke
	 *       {@link #cancel(PointImpl)} during the waiting period (i.e., before ref count of start reaches 0).
	 *       No lock is required here because they are all setting {@code state} to the same value,
	 *       and the requisite HAPPENS BEFORE relation is established by them decrementing the ref count
	 *       of start anyhow.
	 *  <li> Otherwise, reads and writes to state occur only from {@code this}, and only for
	 *       significant "events" like a point reaching zero wait count or occurring.  As these
	 *       events are linked by HAPPENS BEFORE relationships, no locks are needed. 
	 *  </ul> 
	 */
	private State state;
	
	/**
	 * True if our end point was cancelled.  This occurs when some predecessor of our
	 * end point (but not one of our children) dies with an error.  Unlike a predecessor
	 * of our start point, this does not cause our interval to be skipped, but it
	 * does affect the final state of the end point.
	 */
	private boolean endCanceled;
	
	/** Exceptions from this interval or its subintervals that have propagated
	 *  up to us.  
	 *  
	 *  Locking policy:
	 *  <ul>
	 *  <li> Only modified by ourselves or our children.
	 *  <li> During states {@link State#LOCK} and {@link State#RUN}
	 *       this is modified without locks (using {@link #addVertExceptionUnsyc(Throwable)})_, 
	 *       because no children are active.
	 *  <li> During state {@link State#PAR}, only modified by children with a lock (using
	 *       {@link #addVertExceptionSync(Throwable)}.
	 *  <li> During other states, should not be modified becuase it is not possible for
	 *       this interval or a child to generate an uncaught error.  
	 *  </ul> 
	 *  This locking policy is safe because state transitions HAPPENS BEFORE one another.
	 */
	private PSet<Throwable> vertExceptions;

	/** Linked list of locks to acquire before executing.  Modified only when
	 *  synchronized, but once start point occurs can now longer be modified,
	 *  so used without synchronization in methods that must happen after start. 
	 *  <b>Warning:</b> Stored in THE REVERSE ORDER of how they should be acquired! */
	private LockList revLocks;
	
	/** Any child intervals created before run method finishes execution.
	 * 
	 *  Once we transition to {@link State#PAR}, set to null and never
	 *  changed. Modified only under lock! */
	private ChunkList<IntervalImpl> pendingChildIntervals = null;
	
	IntervalImpl(
			IntervalImpl parent, 
			Task task, 
			int pntFlags
	) {
		// Careful: invoke before adding interval to any
		// tables, so if it fails we have no cleanup.
		String taskName = task.getName();
		
		Current current = Current.get();
		
		// If parent == null, then direct child of root interval:
		PointImpl parentEnd = 
			(PointImpl) Intervals.getEnd(parent);
		
		// Note: if this check passes, then no need to check for cycles 
		// for the path from current.start->bnd.  This is because we require
		// one of the following three conditions to be true, and in all three
		// cases a path current.start->bnd must already exist:
		// (1) Bound was created by us and is unscheduled.  Then a
		//     path current.start->bnd was already added.
		// (2) Bound is current.end.  current.start->current.end.
		// (3) A path exists from current.end -> bnd.  Same as (2).
		if(parent != null)
			current.checkCanAddChild(parent);
		
		this.name = taskName;
		this.parent = parent;
		this.task = task;
		end = new PointImpl(name, pntFlags | PointImpl.FLAG_END, parentEnd, 2, this);
		start = new PointImpl(name, pntFlags, end, 2, this);		

		State parentState;
		if(parent != null)
			parentState = parent.addChildInterval(this);
		else 
			parentState = State.PAR; // Root interval always in PAR state.
		
		if(!parentState.permitsNewChildren)
			throw new IntervalException.ParPhaseCompleted(parent);			
		if(!parentState.willSignalChild)
			start.addWaitCountUnsync(-1);
		if(parentState.childrenCancelled)
			state = State.CANCEL_WAIT;
		else 
			state = State.WAIT;
		
		unscheduled = current;
		current.addUnscheduled(this);
				
		ExecutionLog.logNewInterval(null, start, end);
		
		task.attachedTo(this);
	}
	
	/** Moves to a new state. */
	private void transitionUnsync(State newState) {
		if(Debug.ENABLED)
			Debug.transition(this, state, newState);
		state = newState;
	}
	
	private State addChildInterval(IntervalImpl inter) {
		State state;
		
		// Atomically read current state and add 'inter' to our linked
		// list of pending child intervals if appropriate: 
		synchronized(this) {
			state = this.state;
			if(state.willSignalChild)
				pendingChildIntervals = ChunkList.add(pendingChildIntervals, inter, ChunkList.NO_FLAGS);
		}
		
		// Add to end's wait count if we are in a valid state to accept children:
		if(state.permitsNewChildren)
			end.addWaitCount();
		
		return state;
	}
	
	synchronized void cancel(PointImpl pnt) {
		// See declaration for state field for a discussion of why no lock is needed here.
		if(pnt == start) {
			switch(state) {
			case WAIT:
				transitionUnsync(State.CANCEL_WAIT);
				break;
			case CANCEL_WAIT:
				return;
			default:
				throw new IntervalException.InternalError("cancel() of start in unexpected state " + state);
			}
		} else {
			endCanceled = true;
		}
	}

	void addVertExceptionUnsyc(Throwable thr) {
		if(vertExceptions == null) 
			vertExceptions = Empty.set();
		vertExceptions = vertExceptions.plus(thr);
	}
	
	void addVertExceptionsUnsyc(PSet<Throwable> errors) {
		if(vertExceptions == null) 
			vertExceptions = errors;
		else 
			vertExceptions = vertExceptions.plusAll(errors);
	}
	
	synchronized void addVertExceptionSync(Throwable thr) {
		addVertExceptionUnsyc(thr);
	}
	
	private synchronized void addVertExceptionsSync(PSet<Throwable> errors) {
		addVertExceptionsUnsyc(errors);
	}

	void setRevLocksUnsync(LockList locks) {
		revLocks = locks;
	}
	
	/** Note: Safety checks apply!  See {@link Current#checkCanAddDep(PointImpl)} */ 
	void addExclusiveLock(LockImpl lockImpl, Guard guard) {
		LockList list = new LockList(this, lockImpl, guard, null);
		synchronized(this) {
			list.next = revLocks;			
			setRevLocksUnsync(list);
		}
	}
	
	synchronized LockList revLocksSync() {
		return revLocks;
	}
	
	
	private PSet<Throwable> makePSet(
			Set<? extends Throwable> catchErrors,
			PSet<Throwable> errors) 
	{
		if(catchErrors == null)
			return null;
		if(catchErrors == errors)
			return errors;
		if(catchErrors.isEmpty())
			return null;
		return HashTreePSet.from(catchErrors);
	}
	
	/** Invoked by {@code point} when its wait count reaches zero. 
	 *  Should eventually invoke {@link PointImpl#occur(boolean)} */
	void didReachWaitCountZero(PointImpl pointImpl) {
		switch(state) {
		case WAIT:
			assert pointImpl == start;
			transitionUnsync(State.LOCK);
			acquireNextUnacquiredLock();
			break;
			
		case CANCEL_WAIT:
			assert pointImpl == start;
			// Transition to CANCEL_PAR occurs once start point occurred.
			// This way it occurs atomically with reading list of pending intervals.
			start.occur(true);
			break;
			
		case PAR:
		case CATCH_PAR:
			assert pointImpl == end;
			if(vertExceptions != null) {
				transitionUnsync(State.CATCH);
				
				// Can safely read vertExceptions field without locks 
				// because all children have finished and no more can be created:
				PSet<Throwable> errors = vertExceptions;
				vertExceptions = null; // no need to keep a pointer to this no more
				
				Current cur = Current.push(this);
				
				errors = catchErrors(errors);
				cur.pop();
				
				boolean hasUncaughtExceptions = (errors != null);
				if(hasUncaughtExceptions) { // Uncaught exceptions?
					if(parent != null) { // Propogate to parent.
						parent.addVertExceptionsSync(errors);
					} else { // If no parent, throw 'em and let the user sort it out, I guess.
						throw new RethrownException(errors);
					}
				} 
				end.occur(hasUncaughtExceptions || endCanceled);
			} else {
				assert state == State.PAR;
				end.occur(endCanceled);
			}
			break;
			
		case CANCEL_PAR:
			assert pointImpl == end;
			end.occur(true);
			break;
						
		default:
			assert pointImpl == start || pointImpl == end;
			throw new IntervalException.InternalError(
					String.format("Invalid state %s when %s point %s reached wait count 0",
							state, (pointImpl == start ? "start" : "end"), pointImpl));
		}
	}
	
	protected PSet<Throwable> catchErrors(PSet<Throwable> errors) {
		if(errors == null)
			return null;
		
		try { // Execute catchErrors():
			return makePSet(task.catchErrors(errors), errors);
		} catch(Throwable t) {
			return errors.plus(t); // keep previous errors, what the heck.
		}
	}

	/** Acquires the next unacquired lock, invoking {@link PointImpl#occur(boolean)}
	 *  when complete.  If some lock cannot be immediately acquired,
	 *  then it returns and waits for a callback when the lock has
	 *  been acquired. */
	void acquireNextUnacquiredLock() {
		assert state == State.LOCK;
		
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
		
		// Finished acquiring locks.
		start.occur(false);
	}
	
	/** Invoked by {@code ll} when it has acquired a lock.  Checks for data 
	 *  race errors and then tries to acquire the next lock. */
	void didAcquireLock(LockList ll) {
		assert ll.acquiredLock != null;
		
		// Check that acquiring this lock did not
		// create a data race:		
		Throwable err = (ll.guard == null ? null : ll.guard.checkLockable(this, ll.lockImpl));
		if(err != null)
			addVertExceptionUnsyc(err);
		
		acquireNextUnacquiredLock();
	}

	/** Tries to acquire the lock indicated by {@code thisLockList}: first determines 
	 *  if this is a recursive acquire by checking for an ancestor which has already 
	 *  acquired {@code thisLockList}. If one is found, tries to acquire the parent's lock.  
	 *  Otherwise, tries to acquire the {@link LockList#lockImpl} of {@code thisLockList} 
	 *  directly.
	 *  
	 *  @param thisLockList the {@link LockList} entry we are acquiring
	 */
	private int acquireLock(LockList thisLockList) {
		// First check whether we are recursively acquiring this lock:
		for(IntervalImpl ancestor = parent; ancestor != null; ancestor = ancestor.parent) {
			for(LockList ancLockList = ancestor.revLocks; ancLockList != null; ancLockList = ancLockList.next)
				if(ancLockList.lockImpl == thisLockList.lockImpl) {
					return ancLockList.tryAndEnqueue(thisLockList);
				}
		}
		
		// If not:
		return thisLockList.lockImpl.tryAndEnqueue(thisLockList);							
	}

	/** Invoked by {@code pnt} when it has occurred. */
	final void didOccur(PointImpl pnt) {
		assert pnt.didOccur();
		switch(state) {
		case LOCK:
			assert pnt == start;
			if(vertExceptions == null) { // No errors acquiring locks.
				transitionUnsync(State.RUN);
				ContextImpl.POOL.submit(this);
			} else { // Errors while acquiring locks: skip run() method.
				finishSequentialPortion(State.CATCH_PAR);
			}
			break;
		
		case CANCEL_WAIT:
			assert pnt == start;
			finishSequentialPortion(State.CANCEL_PAR);
			break;
			
		case CATCH:
		case PAR:
		case CANCEL_PAR:
			assert pnt == end;
			if(pnt.didOccurWithError())
				transitionUnsync(State.ERROR_END);
			else
				transitionUnsync(State.END);

			// After end point occurs, release any locks we acquired.
			assert pnt == end;
			for(LockList lock = revLocks; lock != null; lock = lock.next)
				// If there were pending exceptions, we may not have acquired any locks:
				if(lock.acquiredLock != null)
					lock.unlockAcquiredLock();
			break;
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
					task.run(this);
				} catch(Throwable t) {
					addVertExceptionUnsyc(t);
				}
				cur.schedule();				
				
				finishSequentialPortion((vertExceptions == null ? State.PAR : State.CATCH_PAR));
			} catch(Throwable e) {
				e.printStackTrace(); // unexpected!
			}
		} finally { // I don't expect any exceptions, but...
			cur.pop();
		}
	}
	
	/** Finishes the sequential portion of the interval and enters the
	 *  parallel portion in the given state, starting any pending
	 *  child intervals stored in {@link #pendingChildIntervals}.  
	 *  If the new state is an error state, then our child intervals 
	 *  will be cancelled before we start them. */
	private void finishSequentialPortion(final State newState) {
		ChunkList<IntervalImpl> pending;
		synchronized(this) {
			transitionUnsync(newState);
			pending = pendingChildIntervals;
			this.pendingChildIntervals = null;
		}
		
		if(pending != null) {
			new ChunkList.Iterator<IntervalImpl>(pending) {
				@Override public void doForEach(IntervalImpl child, int flags) {
					if(newState.childrenCancelled)
						child.cancel(child.start);
					child.start.arrive(1);
				}
			};
		}

		end.arrive(1);
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