package ch.ethz.intervals.impl;

import java.io.IOException;
import java.util.Set;

import pcollections.Empty;
import pcollections.HashTreePSet;
import pcollections.PSet;
import ch.ethz.intervals.AsyncInterval;
import ch.ethz.intervals.Condition;
import ch.ethz.intervals.Interval;
import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.Lock;
import ch.ethz.intervals.Point;
import ch.ethz.intervals.RethrownException;
import ch.ethz.intervals.RoInterval;
import ch.ethz.intervals.RoLock;
import ch.ethz.intervals.RoPoint;
import ch.ethz.intervals.Task;
import ch.ethz.intervals.guard.Guard;
import ch.ethz.intervals.impl.ThreadPool.Medallion;
import ch.ethz.intervals.util.ChunkList;

import com.smallcultfollowing.lathos.Lathos;
import com.smallcultfollowing.lathos.Output;
import com.smallcultfollowing.lathos.Page;
import com.smallcultfollowing.lathos.PageContent;

public abstract class IntervalImpl 
extends ThreadPool.WorkItem 
implements Guard, Interval, Page, RefManipulator
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

	@Override
	public Condition condReadableBy() {
		return end.condDidOccurWithoutError();
	}

	@Override
	public Condition condWritableBy() {
		return this.condIsInlineSubintervalOfOrEqualTo();
	}
	
	@Override
	public Condition condFinal() {
		return end.condDidOccurWithoutError();
	}
	
	@Override
	public Condition condSubintervalOfOrEqualTo() {
		return new Condition() {
			@Override
			public boolean isTrueFor(RoPoint mr, RoInterval current) {
				return current.isSubintervalOfOrEqualTo(IntervalImpl.this);
			}

			@Override
			public String description() {
				return String.format("subOf(%s)", IntervalImpl.this);
			}
		};
	}

	@Override
	public Condition condIsInlineSubintervalOfOrEqualTo() {
		return new Condition() {
			@Override
			public boolean isTrueFor(RoPoint mr, RoInterval current) {
				return current.isInlineSubintervalOfOrEqualTo(IntervalImpl.this);
			}

			@Override
			public String description() {
				return String.format("inlineSubOf(%s)", IntervalImpl.this);
			}
		};
	}

	@Override
	public boolean isSubintervalOfOrEqualTo(RoInterval inter) {
		if(inter == this)
			return true;
		
		if(inter == null)
			return true;

		if(parent == null)
			return false;
		
		return parent.isSubintervalOfOrEqualTo(inter);
	}

	@Override
	public boolean isInlineSubintervalOfOrEqualTo(RoInterval inter) {
		if(inter == this)
			return true;
		
		if(!isInline())
			return false;
		
		if(parent == null)
			return false;
		
		return parent.isInlineSubintervalOfOrEqualTo(inter);
	}

	/**
	 * True if {@code this} will hold the lock {@code lock}
	 * when it executes, or it is a blocking subinterval of
	 * someone who will.
	 */
	@Override public final boolean locks(RoLock lock, Guard guard) {
		if(holdsLockItself(lock, guard))
			return true;
		
		if(isInline() && parent != null)
			return parent.locks(lock, guard);
		
		return false;
	}
	
	/**
	 * True if {@code this} will hold the lock {@code lock}
	 * when it executes.
	 */
	final boolean holdsLockItself(RoLock lock, Guard guard) {
		LockRecord rec;
		synchronized(this) {
			rec = records;
		}
		for(; rec != null; rec = rec.nextForInterval()) {
			if(rec.matches(lock, guard))
				return true;
		}
		return false;
	}
	
	@Override public void addHb(Point to) {
		end.addHb(to);
	}
	
	@Override public void addHb(Interval to) {
		end.addHb(to);
	}
	
	@Override public Point getStart() {
		return start;
	}
	
	@Override public Point getEnd() {
		return end;
	}
	
	@Override public void addLock(Lock lock) {
		addLock(lock, lock);
	}
	
	@Override public void addLock(Lock lock, Guard guard) {
		addLock(start, lock, guard);
	}
	
	@Override public void addLock(Point _acq, Lock _lock, Guard guard) {
		PointImpl acq = (PointImpl) _acq;
		LockImpl lock = (LockImpl) _lock;
		
		assert acq != null && lock != null && guard != null;
		
		if(!acq.hbeq(start)) {
			throw new IntervalException.MustHappenBefore(acq, start);
		}
			
		Current current = Current.get();
		current.checkCanAddDep(acq);
		
		addExclusiveLock(acq, lock, guard);
	}

	@Override
	public IntervalImpl getParent() {
		return parent;
	}

	@Override
	public AsyncInterval newAsyncChild(Task task) {
		return new AsyncIntervalImpl(this, task);
	}

	// =====================================================================================
	// Package or private interface:
	//
	// Note: I have tried to aggressively label methods as final or private unless
	// they are overridden in a subclass or used outside of this class.  This is not
	// necessarily indicative of a *design constraint* so much as intended for 
	// documentation of where a function is used.
	
	public final IntervalImpl parent;
	public final PointImpl start;
	public final PointImpl end;
	public final Task task;
	
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
		// LOCK(PERMITS_CHILDREN | WILL_SIGNAL_CHILDREN), (points do the locking now)
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
		public final boolean willSignalAsyncChild;
		
		/** True if children creates in this state should be CANCELLED */
		public final boolean childrenCancelled;

		private State(int flags) {			
			this.permitsNewChildren = (flags & PERMITS_CHILDREN) != 0;
			this.willSignalAsyncChild = (flags & WILL_SIGNAL_CHILDREN) != 0;
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
	 *  <li> During state {@link State#RUN} this is modified without locks
	 *       (using {@link #addVertExceptionUnsync(Throwable)})_, 
	 *       because no children are active.
	 *  <li> During state {@link State#PAR}, only modified by children with a lock (using
	 *       {@link #addVertExceptionSync(Throwable)}.
	 *  <li> During other states, should not be modified becuase it is not possible for
	 *       this interval or a child to generate an uncaught error.  
	 *  </ul> 
	 *  This locking policy is safe because state transitions HAPPENS BEFORE one another.
	 */
	private PSet<Throwable> vertExceptions;

	/** Linked list of locks that will be held while executing.
	 *  Until the start point occurs, only accessed with synchronization.
	 *  After that point, accessible without lock. */  
	private LockRecord records;
	
	/** Any child intervals created before run method finishes execution.
	 * 
	 *  Once we transition to {@link State#PAR}, set to null and never
	 *  changed. Modified only under lock! */
	private ChunkList<IntervalImpl> pendingChildIntervals = null;
	
	IntervalImpl(
			IntervalImpl parent, 
			Task task
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
		
		boolean isInline = isInline();
		
		final int pntFlags = (isInline ? PointImpl.FLAG_INLINE : PointImpl.NO_FLAGS);
		final int startWaitCount = 2; // parent, sched
		final int endWaitCount = 2; // start, task

		this.name = taskName;
		this.parent = parent;
		this.task = task;
		end = new PointImpl(name, pntFlags | PointImpl.FLAG_END, parentEnd, endWaitCount, this);
		start = new PointImpl(name, pntFlags, end, startWaitCount, this);
		
		State parentState;
		if(parent != null)
			parentState = parent.addChildInterval(this);
		else 
			parentState = State.PAR; // Root interval always in PAR state.
		
		if(!parentState.permitsNewChildren)
			throw new IntervalException.ParPhaseCompleted(parent);			
		
		if(isInline || !parentState.willSignalAsyncChild) {
			start.addWaitCountUnsync(-1);			
		}
		
		if(parentState.childrenCancelled)
			state = State.CANCEL_WAIT;
		else 
			state = State.WAIT;
		
		unscheduled = current;
		current.addUnscheduled(this);
				
		ExecutionLog.logNewInterval(null, start, end);
		
		if(Debug.ENABLED) {
			// There may be a bit of a race cond. here between the
			// parent starting, and thus posting a DecRef, and us posting
			// the AddRef event.  But hey, that's life.  Also, I think that in
			// the new rules the creator of a child must HB the start of the
			// parent, in which case there is no danger.
			
			if(!isInline && parentState.willSignalAsyncChild)
				Debug.debug.postAddRef(start, parent, 1);
			Debug.debug.postAddRef(start, current.inter, 1);
				
			Debug.debug.postAddRef(end, start, 1);
			Debug.debug.postAddRef(end, this, 1);
			
			Debug.debug.postNewInterval(this, current.inter);
		}
		
		// Note: task.attachedTo() is invoked in the subclasses.
	}
	
	/** Moves to a new state. */
	private void transitionUnsync(State newState) {
		if(Debug.ENABLED)
			Debug.debug.postTransition(this, state, newState);
		state = newState;
	}
	
	private State addChildInterval(IntervalImpl inter) {
		State state;
		boolean isInline = inter.isInline();
		
		// Atomically read current state and add 'inter' to our linked
		// list of pending child intervals if appropriate: 
		synchronized(this) {
			state = this.state;
			if(!isInline && state.willSignalAsyncChild)
				pendingChildIntervals = ChunkList.add(pendingChildIntervals, inter, ChunkList.NO_FLAGS);
		}
		
		// If we accept the new child, then wait for it to end
		if(state.permitsNewChildren)
			end.addWaitCount(inter.end);
		
		return state;
	}
	
	synchronized final void cancel(PointImpl pnt) {
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
	
	final void addVertExceptionUnsync(Throwable thr) {
		if(vertExceptions == null) 
			vertExceptions = Empty.set();
		
		if(thr instanceof RethrownException) {
			RethrownException rethr = (RethrownException) thr;
			vertExceptions = vertExceptions.plusAll(rethr.allErrors());
		} else {
			vertExceptions = vertExceptions.plus(thr);
		}
		
		if(Debug.ENABLED)
			Debug.debug.postVertException(this, thr, vertExceptions);
	}
	
	synchronized final void addVertExceptionSync(Throwable thr) {
		addVertExceptionUnsync(thr);
	}
	
	private synchronized void addVertExceptionsSync(PSet<Throwable> errors) {
		if(vertExceptions == null) 
			vertExceptions = errors;
		else 
			vertExceptions = vertExceptions.plusAll(errors);
		
		if(Debug.ENABLED)
			Debug.debug.postVertException(this, null, vertExceptions);
	}

	/** Note: Safety checks apply!  See {@link Current#checkCanAddDep(PointImpl)} */ 
	final void addExclusiveLock(PointImpl acq, LockImpl lock, Guard guard) {
		LockRecord record = new LockRecord(lock, guard, acq, end);
		
		// Check for recursive lock:
		//    Note that set of locks held by parent is 
		//    fixed at this point.
		if(isInline()) {
			if(parent != null && parent.locks(lock, null))
				record.setRecursive();
		}
		
		synchronized(this) {
			records = record.insertForInterval(records);
		}
		record.acquiredBy.addLockRecord(record);
		record.releasedBy.addLockRecord(record);
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
	final void didReachWaitCountZero(PointImpl pointImpl) {
		switch(state) {
		case WAIT:
			assert pointImpl == start;
			start.occur(false);
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
		
		try { 
			// Execute catchErrors() from task.  If it itself
			// fails, tack those new errors onto the old ones.
			return makePSet(task.catchErrors(errors), errors);
		} catch(RethrownException rethr) {
			return errors.plusAll(rethr.allErrors());
		} catch(Throwable t) {
			return errors.plus(t); 
		}
	}

	/** Invoked by {@code pnt} when it has occurred. */
	final void didOccur(PointImpl pnt) {
		assert pnt.didOccur();
		switch(state) {
		case WAIT:
			assert pnt == start;
			
			// Check that acquiring locks did not create data races:
			//for(LockRecord record = records; record != null; record = record.nextForInterval()) {
			//	Throwable err = record.guard.checkLockable(record.acquiredBy, this, record.lock);
			//	if(err != null)
			//		addVertExceptionUnsync(err);
			//}

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
			break;
			
		default:
			throw new IntervalException.InternalError("Invalid state: " + state);
		}
	}
	
	/**
	 * The "main" method for this interval: invoked when we are scheduled.
	 * Simply invokes {@link #exec()}.
	 */
	@Override
	final void exec(Medallion medallion) {
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
				// Execute the task and add any uncaught
				// exceptions to vertExceptions:
				boolean endedNormally;
				try {
					task.run(this);
					endedNormally = true;
				} catch(Throwable t) {
					addVertExceptionUnsync(t);
					endedNormally = false;
				}
				
				// Schedule/cancel any unscheduled subintervals.  
				// n.b.: This may add to vertExceptions!
				if(endedNormally)
					cur.scheduleAll();
				else
					cur.cancelAll();
				
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
					child.start.arrive(1, IntervalImpl.this);
				}
			};
		}

		end.arrive(1, this);
	}

	/**
	 * Returns true if this interval is due to be scheduled by
	 * {@cure current}.
	 */
	final boolean isUnscheduled(Current current) {
		// No need to worry about race conditions: if our field == current,
		// then current is the only thread that will ever change it.
		// Otherwise, going to return false anyhow.
		return (unscheduled == current);
	}

	/** 
	 * Invoked by {@link Current} when the interval is scheduled.
	 * 
	 * Overridden by {@link InlineIntervalImpl#didSchedule(boolean)} to 
	 * check that inline intervals are never implicitly scheduled.
	 * 
	 * @param explicit true if the interval was explicitly scheduled
	 * by the user, false if it is being scheduled implicitly at
	 * the end of the creator.
	 */
	void didSchedule(boolean explicit) {
		// Only invoked by our scheduler:
		unscheduled = null;
	}

	// =====================================================================================
	// Lathos routines
	
	@Override
	public void renderInLine(Output output) throws IOException {
		Lathos.renderInLine(this, output);
	}

	// This routine is not synchronized; its nature is inherently racy.
	@Override
	public void renderInPage(Output out) throws IOException {
		out.startPage(this);

		out.startPar();
		out.startBold();
		out.outputText("Interval: ");
		out.outputText(toString());
		out.endBold();
		out.endPar();

		out.startTable();
		Lathos.headerRow(out, "Field", "Value", "Comments");
		Lathos.row(out, "Name", name, "");
		Lathos.row(out, "Parent", parent, null);
		Lathos.row(out, "Start Point", start, start.didOccur());
		Lathos.row(out, "End Point", end, start.didOccur());
		Lathos.row(out, "State", state, "");
		out.endTable();
		
		Debug.debug.renderEventsForObject(out, this);
		
		out.endPage(this);
	}

	@Override
	public String getId() {
		return Lathos.defaultId(this);
	}

	@Override
	public void addContent(PageContent content) {
		throw new UnsupportedOperationException();
	}
	
}
