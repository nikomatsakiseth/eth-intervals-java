package ch.ethz.intervals;

import static ch.ethz.intervals.EdgeList.NORMAL;
import static ch.ethz.intervals.EdgeList.SPECULATIVE;



public class Intervals {
	
	public static class NamedTask extends AbstractTask {
		public final String name;
		public NamedTask(String name) {
			super();
			this.name = name;
		}
		public void run(Point currentEnd) {
		}
		public String toString() {
			return name;
		}		
	}
	
	/** Convenient task that does nothing. */
	public static final Task emptyTask = new NamedTask("emptyTask");
	static final Task readTask = new NamedTask("readTask");

	static final PointImpl ROOT_END = new PointImpl(1); // never occurs
	
	static final ThreadPool POOL = new ThreadPool();
	
	public static Task namedTask(String name) {
		return new NamedTask(name);
	}
	
	/** 
	 * Returns {@code i.start()} unless {@code i} is null, 
	 * in which case just returns null. */
	public static Point start(Interval i) {
		if(i == null)
			return null;
		return i.start();
	}
	
	/** 
	 * Returns {@code i.end()} unless {@code i} is null, 
	 * in which case just returns null. */
	public static Point end(Interval i) {
		if(i == null)
			return null;
		return i.end();
	}

	/** 
	 * Creates and returns a new unscheduled interval with no bound.
	 * You can add additional dependencies by invoking methods on the
	 * {@code UnscheduledInterval} object.  To obtain the new interval,
	 * it must be scheduled using {@link UnscheduledInterval#schedule(Task)}.
	 * 
	 * Once scheduled, the resulting interval will execute in 
	 * parallel with the caller.  To create a blocking interval, use
	 * {@link #blockingInterval(Task)}.
	 * 
	 * @see #intervalWithBound(Point)
	 * @see #blockingInterval(Task)
	 */
	public static Interval interval(Task task) {
		return intervalWithBound(ROOT_END, task);
	}
	
	/**
	 * Returns an interval whose bound is the end of the current interval.
	 */
	public static Interval childInterval(Task task) {
		// No need for safety checks, it's always legal to create a child interval:
		Current current = Current.get();
		return intervalWithBoundUnchecked(current.end, task, current);
	}

	/**
	 * Returns an interval whose bound is the same as the bound of the current interval.
	 * However, If the current interval is the root interval, then the bound is the end of
	 * the root interval.
	 */
	public static Interval siblingInterval(Task task) {
		Current current = Current.get();
		return siblingInterval(current, task);
	}

	static Interval siblingInterval(Current current, Task task) {
		Point bound;
		if(current.end == ROOT_END)
			bound = current.end;
		else
			bound = current.end.bound;
		// No need for safety checks, it's always legal to create a sibling interval:
		return intervalWithBoundUnchecked(bound, task, current);
	}
	
	/**
	 * Like {@link #siblingInterval(Task)}, but the returned interval does
	 * not begin until the current interval has completed.
	 */
	public static Interval successorInterval(Task task) {
		Current current = Current.get();
		Interval result = siblingInterval(current, task);
		
		// XXX Depending on the dependencies specified in task, 
		// this edge may or may not be legal!  We should really
		// add it before invoking task.addDependencies(),
		// so that any error would occur THERE and not HERE,
		// but that requires reshuffling our APIs a bit.
		Intervals.addHb(current.end, result.start());
		
		return result;
	}
	
	/** 
	 * Creates and returns a new unscheduled interval with a bound
	 * {@code bnd}.  A bound is effectively a dependency {@code endBefore(bnd)},
	 * but has additional ramifications when doing dynamic race detection.
	 * 
	 * You can add additional dependencies by invoking methods on the
	 * {@code UnscheduledInterval} object.  To obtain the new interval,
	 * it must be scheduled using {@link UnscheduledInterval#schedule(Task)}.
	 *  
	 * Once scheduled, the resulting interval will execute in 
	 * parallel with the caller.  To create a blocking interval, use
	 * {@link #blockingInterval(Task)}.
	 * 
	 * @see #interval()
	 * @see #intervalDuring(Interval)
	 * @see #blockingInterval(Task)
	 */
	public static Interval intervalWithBound(Point bnd, Task task) {
		Current current = Current.get();
		return intervalWithBound(current, bnd, task);
	}

	static Interval intervalWithBound(Current current, Point bnd, Task task) {
		current.checkCanAddHb(current.start, bnd);		
		IntervalImpl result = intervalWithBoundUnchecked(bnd, task, current);		
		return result;
	}
	
	static IntervalImpl intervalWithBoundUnchecked(
			Point bnd,
			Task task, 
			Current current) 
	{
		PointImpl bndImpl = (PointImpl) bnd;
		bndImpl.addWaitCount(); // now waiting for end
		PointImpl end = new PointImpl(current, bndImpl, 2); // waiting for task, start 
		PointImpl start = new PointImpl(current, end, 1);   // waiting to be scheduled
		if(current.start != null) // current.start must have occurred, so can't affect WC
			current.start.justAddEdgeWithoutAdjusting(start, NORMAL);
		IntervalImpl result = new IntervalImpl(task, start, end);		
		start.setWorkItemBeforeScheduling(result);
		ExecutionLog.logNewInterval(current.start, start, end);
		ExecutionLog.logScheduleInterval(start, task); // XXX this event is no longer logically separate
		current.addUnscheduled(result);
		return result;
	}
	
	/** 
	 * Creates and returns a new unscheduled interval with a bound
	 * {@code interval.end()} and which always starts after
	 * {@code interval.start()}.
	 * 
	 * You can add additional dependencies by invoking methods on the
	 * {@code UnscheduledInterval} object.  To obtain the new interval,
	 * it must be scheduled using {@link UnscheduledInterval#schedule(Task)}.
	 *  
	 * Once scheduled, the resulting interval will execute in 
	 * parallel with the caller.  To create a blocking interval, use
	 * {@link #blockingInterval(Task)}.
	 * 
	 * @see #interval()
	 * @see #intervalWithBound(Point) 
	 * @see #blockingInterval(Task)
	 */
	public static Interval intervalDuring(Interval interval, Task task) {
		Interval result = intervalWithBound(interval.end(), task);
		addHb(interval.start(), result.start());
		return result;
	}
	
	/** 
	 * Creates a dependency so that {@code from} <em>happens before</em>
	 * {@code to}.  
	 * 
	 * Invoking this method is only legal when one of the following
	 * conditions is true, as they ensure that {@code to}
	 * cannot have occurred yet:
	 * <ul>
	 * <li>{@code to} belongs to an unscheduled interval creates by
	 *     the current task.
	 * <li>{@code to} is the end of the current interval.
	 * <li>The end of the current interval <em>happens before</em> {@code to}.
	 * </ul>
	 * If none of the above conditions are met, then 
	 * the method throws a {@link NoEdgeException}.
	 * 
	 * Furthermore, if {@code to} already <em>happens before</em> {@code from},
	 * then a {@link CycleException} is thrown.
	 * 
	 * @throws NoEdgeException see above.
	 * @throws CycleException see above.
	 */	
	public static void addHb(Point from, Point to) {
		if(to == null)
			return;
		
		/* Subtle:
		 * 
		 * It is rather expensive to guarantee that adding the edge
		 * from->to will not lead to a cycle!  This is because the
		 * check and addition would have to be done atomically
		 * across the whole graph.
		 * 
		 * Consider the following scenario:
		 * 
		 *  i1---+
		 *       |
		 *       v
		 *       a < - - - b
		 *       |         ^
		 *       v         |
		 *       c - - - > d
		 *                 ^
		 *                 |
		 *  i2-------------+
		 * 
		 * The edges b->a and c->d are both being added in parallel
		 * by intervals i1 and i2.  The edges a->c and d->b both exist already.  
		 * Now, these two edges to be added do not share any endpoints, but
		 * together they form a cycle.  If both do a cycle check
		 * simultaneously, they will not find a problem, but then
		 * both could proceed to add and create a problem.
		 * 
		 * There are several possible solutions here.  One technique
		 * would be to accumulate locks during the cycle check and
		 * only release them once the add is complete.  This guarantees
		 * that the region of the graph you care about is modified 
		 * atomically.  A modified version of this algorithm acquires
		 * locks not on the nodes themselves but on the *bound* of the
		 * node.  This would have the effect of segmenting the graph so
		 * that fewer locks are required (no locks would ever be acquired
		 * on leaf nodes, essentially).  You know that this technique is
		 * deadlock free because the graph you are walking is acyclic, as
		 * an invariant.
		 * 
		 * Another technique is to be optimistic: insert the edge, and
		 * then do the check.  If you find a cycle, uninsert the edge and
		 * throw an exception.  
		 * 
		 * To make this easier, we insert the edge with
		 * a flag that marks it as SPECULATIVE.  It will then be ignored 
		 * should the source point occur in the meantime, and also for any
		 * hb() checks the user may perform.  This means that after 
		 * we have confirmed the edge is okay, we have to remove the speculative
		 * mark.  At that time, we also adjust its wait count and 
		 * propagate any exceptions. 
		 */
		
		Current current = Current.get();
		current.checkCanAddDep(to);
		
		if(from == null)
			return;
		
		// Optimistically add edge (though it may cause a cycle!):
		//
		//   If safety checks are enabled, then we initially record
		//   the edge as non-deterministic until we have confirmed
		//   that it causes no problems.
		PointImpl fromImpl = (PointImpl) from;
		PointImpl toImpl = (PointImpl) to;
		
		if(!SAFETY_CHECKS) {
			fromImpl.addEdgeAndAdjust(toImpl, NORMAL);
		} else {
			// Really, these helper methods ought to be inlined,
			// but they are separated to aid in testing. 
			optimisticallyAddEdge(fromImpl, toImpl);
			checkForCycleAndRecover(fromImpl, toImpl);			
		}
		
		ExecutionLog.logEdge(from, to);
	}

	/** Helper method of {@link #addHb(Point, Point)}.
	 *  Pulled apart for use with testing. */
	static void optimisticallyAddEdge(
			PointImpl fromImpl,
			PointImpl toImpl) 
	{
		// Note: the edge is considered speculative until we have
		// verified that the resulting graph is acyclic.
		fromImpl.justAddEdgeWithoutAdjusting(toImpl, SPECULATIVE);
	}
	
	/** Helper method of {@link #addHb(Point, Point)}.
	 *  Pulled apart for use with testing. */
	static void checkForCycleAndRecover(
			PointImpl fromImpl,
			PointImpl toImpl) 
	{
		if(toImpl.hb(fromImpl, 0)) {
			recoverFromCycle(fromImpl, toImpl);
			throw new CycleException(fromImpl, toImpl);
		} else {
			fromImpl.confirmEdgeAndAdjust(toImpl, NORMAL);
		}
	}

	/** Helper method of {@link #addHb(Point, Point)}.
	 *  Pulled apart for use with testing. */
	static void recoverFromCycle(PointImpl fromImpl, PointImpl toImpl) {
		// Uh-oh, error, go into damage control.
		fromImpl.unAddEdge(toImpl);
	}
	
	public static void exclusiveLock(Interval interval, Guard guard) {
		if(interval != null && guard != null) {
			Current current = Current.get();
			PointImpl start = (PointImpl) interval.start();
			current.checkCanAddDep(start);
			start.addPendingLock((GuardImpl) guard, true);
		}
	}

	/**
	 * Schedules any intervals created by the current task that
	 * have not yet been scheduled.  This method is invoked implicitly
	 * when the task terminates.
	 */
	public static void schedule() {
		Current.get().schedule();
	}
	
	/**
	 * @see AsyncPoint
	 */
	public static AsyncPoint asyncPoint(Point bound, int cnt) {
		checkCurrentIntervalEndHbOrSame(bound);
		PointImpl boundImpl = (PointImpl) bound;
		boundImpl.addWaitCount();
		return new AsyncPointImpl(null, boundImpl, cnt);
	}
	
	/**
	 * If set to false, disables all safety checks against
	 * cycles or race conditions.  
	 */
	public static final boolean SAFETY_CHECKS = true;	

	static void checkEdge(Point from, Point to) {
		if (SAFETY_CHECKS && !from.hb(to))
			throw new NoEdgeException(from, to);
	}
	
	static void checkEdgeOrSame(Point from, Point to) {
		if(from != to)
			checkEdge(from, to);
	}

	static void checkCurrentIntervalEndHbOrSame(Point to) {
		if(SAFETY_CHECKS) {
			Current cur = Current.get();
			checkEdgeOrSame(cur.end, to);
		}
	}

	/** Waits for {@code ep} to complete and returns its result.
	 *  Resets the currentInterval afterwards. */
	static void join(PointImpl pnt) {
		if(Debug.ENABLED)
			Debug.join(pnt);
		pnt.join();
		pnt.checkAndRethrowPendingException();
	}
	
	/**
	 * Creates a new interval which executes during the current interval.
	 * This interval will execute {@code task}.  This function does not
	 * return until the new interval has completed.
	 * 
	 * <b>Note:</b> Exceptions that occur in {@code task} are 
	 * wrapped in {@link RethrownException} and rethrown immediately.
	 * Exceptions never propagate to the current interval.
	 */
	public static void blockingInterval(Task task) 
	{		
		// This could be made more optimized, but it will do for now:
		Current current = Current.get();
		IntervalImpl subinterval = intervalWithBoundUnchecked(current.end, task, current);
		subinterval.end.addFlagBeforeScheduling(PointImpl.FLAG_MASK_EXC);
		current.schedule(subinterval);
		join(subinterval.end); // may well throw an exception
	}
	
	/** 
	 * Returns the point which represents the end of the entire
	 * computation.  This point will not occur until all other
	 * points have occurred, and it is the only point without a bound. */
	public static Point rootEnd() {
		return ROOT_END;
	}

}
