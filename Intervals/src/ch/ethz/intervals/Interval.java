package ch.ethz.intervals;

import java.util.Set;

import pcollections.Empty;
import pcollections.HashTreePSet;
import pcollections.PSet;
import ch.ethz.intervals.ThreadPool.Worker;
import ch.ethz.intervals.guard.Guard;
import ch.ethz.intervals.mirror.IntervalMirror;
import ch.ethz.intervals.mirror.LockMirror;
import ch.ethz.intervals.mirror.PointMirror;
import ch.ethz.intervals.quals.Requires;
import ch.ethz.intervals.quals.Subinterval;
import ch.ethz.intervals.util.ChunkList;

public abstract class Interval 
extends ThreadPool.WorkItem 
implements Dependency, Guard, IntervalMirror
{	
	// =====================================================================================
	// Public interface (and some private supporting functions):
	
	public final Interval parent;
	public final Point start;
	public final Point end;
	
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
		if(parent != null)
			current.checkCanAddChild(parent);
		
		this.name = name;
		this.parent = parent;
		end = new Point(name, Point.FLAG_END, parentEnd, 2, this);
		start = new Point(name, Point.NO_FLAGS, end, 2, this);		

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
		
		dep.addHbToNewInterval(this);		
	}
	

	/**
	 * Defines the behavior of the interval.  Must be
	 * overridden. Non-blocking child intervals of {@code this} only 
	 * execute once this method has returned.
	 * 
	 * <p>Executed by the scheduler when {@code this}
	 * is the current interval.  <b>Do not invoke manually.</b> */
	@Requires(subinterval=@Subinterval(of="this"))
	protected abstract void run();
	
	/** Invoked if an error occurs in this interval or in a child.  Gives the
	 *  interval an opportunity to filter and handle errors before propagating
	 *  them onwards.  If you return {@code null} or an empty set, the errors are 
	 *  considered handled and so the interval is considered to terminate without error.
	 *  Otherwise, the returned errors will be propagated to {@link #parent},
	 *  and any successors of {@code this} will not execute.
	 *   
	 *  @param errors An immutable set of errors which occurred.
	 *  @returns the set of errors to propogate forward, or {@code null} for none. */
	protected Set<? extends Throwable> catchErrors(Set<Throwable> errors) {
		return errors;
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

	// =====================================================================================
	// Package or private interface:
	
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
	Interval nextUnscheduled;
	
	/** Current state of this interval.  
	 * 
	 *  Any change to this variable must HAPPEN AFTER the predecessor state.
	 * 
	 *  Locking policy:
	 *  <ul>
	 *  <li> The only time that the state is externally affected is when pred. and parents invoke
	 *       {@link #cancel(Point)} during the waiting period (i.e., before ref count of start reaches 0).
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
	private ChunkList<Interval> pendingChildIntervals = null;
	
	/** Constructor used by blocking subintervals */
	Interval(String name, Interval parent, int pntFlags, int startWaitCount, int endWaitCount) {
		this.state = State.WAIT;
		this.name = name;
		this.parent = parent;		
		this.end = new Point(name, pntFlags | Point.FLAG_END, Intervals.end(parent), endWaitCount, this);
		this.start = new Point(name, pntFlags, end, startWaitCount, this);
		
		if(end.bound != null)
			end.bound.addWaitCount();
	}
	
	/** Moves to a new state. */
	private void transitionUnsync(State newState) {
		if(Debug.ENABLED)
			Debug.transition(this, state, newState);
		state = newState;
	}
	
	private State addChildInterval(Interval inter) {
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
	
	synchronized void cancel(Point pnt) {
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
	 *  Should eventually invoke {@link Point#occur(boolean)} */
	void didReachWaitCountZero(Point point) {
		switch(state) {
		case WAIT:
			assert point == start;
			transitionUnsync(State.LOCK);
			acquireNextUnacquiredLock();
			break;
			
		case CANCEL_WAIT:
			assert point == start;
			// Transition to CANCEL_PAR occurs once start point occurred.
			// This way it occurs atomically with reading list of pending intervals.
			start.occur(true);
			break;
			
		case PAR:
		case CATCH_PAR:
			assert point == end;
			if(vertExceptions != null) {
				transitionUnsync(State.CATCH);
				
				// Can safely read vertExceptions field without locks 
				// because all children have finished and no more can be created:
				PSet<Throwable> errors = vertExceptions;
				vertExceptions = null; // no need to keep a pointer to this no more
				
				Current cur = Current.push(this);
				
				try { // Execute catchErrors():
					if(errors != null)
						errors = makePSet(catchErrors(errors), errors);
				} catch(Throwable t) {
					errors = errors.plus(t); // keep previous errors, what the heck.
				}
				
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
			assert point == end;
			end.occur(true);
			break;
						
		default:
			assert point == start || point == end;
			throw new IntervalException.InternalError(
					String.format("Invalid state %s when %s point %s reached wait count 0",
							state, (point == start ? "start" : "end"), point));
		}
	}

	/** Acquires the next unacquired lock, invoking {@link Point#occur(boolean)}
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
		Throwable err = (ll.guard == null ? null : ll.guard.checkLockable(this, ll.lock));
		if(err != null)
			addVertExceptionUnsyc(err);
		
		acquireNextUnacquiredLock();
	}

	/** Tries to acquire the lock indicated by {@code thisLockList}: first determines 
	 *  if this is a recursive acquire by checking for an ancestor which has already 
	 *  acquired {@code thisLockList}. If one is found, tries to acquire the parent's lock.  
	 *  Otherwise, tries to acquire the {@link LockList#lock} of {@code thisLockList} 
	 *  directly.
	 *  
	 *  @param thisLockList the {@link LockList} entry we are acquiring
	 */
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

	/** Invoked by {@code pnt} when it has occurred. */
	final void didOccur(Point pnt) {
		assert pnt.didOccur();
		switch(state) {
		case LOCK:
			assert pnt == start;
			if(vertExceptions == null) { // No errors acquiring locks.
				transitionUnsync(State.RUN);
				Intervals.POOL.submit(this);
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
					run();
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
		ChunkList<Interval> pending;
		synchronized(this) {
			transitionUnsync(newState);
			pending = pendingChildIntervals;
			this.pendingChildIntervals = null;
		}
		
		if(pending != null) {
			new ChunkList.Iterator<Interval>(pending) {
				@Override public void doForEach(Interval child, int flags) {
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
