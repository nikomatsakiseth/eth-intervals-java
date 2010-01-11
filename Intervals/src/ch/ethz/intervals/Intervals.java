package ch.ethz.intervals;

import static ch.ethz.intervals.EdgeList.NORMAL;
import static ch.ethz.intervals.Point.NO_POINT_FLAGS;

/** Static methods for creating and manipulating intervals. */
public class Intervals {
	
	/**
	 * If set to false, disables all safety checks against
	 * cycles or race conditions.  
	 */
	public static final boolean SAFETY_CHECKS = true;	

	/** Final point of the program.  Never occurs until program ends. */
	static final Line rootLine = Line.rootLine();
	static final Point rootEnd = new Point(rootLine, null, null, NO_POINT_FLAGS, 1, null);
	static final Point rootStart = new Point(rootLine, rootEnd, rootEnd, NO_POINT_FLAGS, Point.OCCURRED, null);
	static final Interval rootInter = new EmptyInterval(null, rootStart, rootEnd, "root");
	
	
	/** Shared thread pool that executes tasks. */
	static final ThreadPool POOL = new ThreadPool();
	
	/** 
	 * Returns {@link Interval#start} unless {@code i} is null, 
	 * in which case just returns {@code null}. */
	public static Point start(Interval i) {
		if(i == null)
			return null;
		return i.start;
	}
	
	/** 
	 * Returns {@link Interval#end} unless {@code i} is null, 
	 * in which case just returns {@code null}. */
	public static Point end(Interval i) {
		if(i == null)
			return null;
		return i.end;
	}
	
	public static Dependency child() {
		final Current current = Current.get();
		return current.inter;
	}
	
	public static Dependency successor() {
		final Current current = Current.get();
		if(current.inter.parent == null)
			throw new NotInRootIntervalException();
		return new Dependency() {			
			@Override
			public Interval parentForNewInterval() {
				return current.inter.parent;
			}
			
			@Override
			public void addHbToNewInterval(Interval inter) {	
				Intervals.addHb(current.end, inter.start);
			}
		};		
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
	 * the method throws a {@link EdgeNeededException}.
	 * 
	 * Furthermore, if {@code to} already <em>happens before</em> {@code from},
	 * then a {@link CycleException} is thrown.
	 * 
	 * @throws EdgeNeededException see above.
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
		Point fromImpl = (Point) from;
		Point toImpl = (Point) to;
		
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
			Point fromImpl,
			Point toImpl) 
	{
		// Note: the edge is considered speculative until we have
		// verified that the resulting graph is acyclic.
		fromImpl.addSpeculativeEdge(toImpl, NORMAL);
	}
	
	/** Helper method of {@link #addHb(Point, Point)}.
	 *  Pulled apart for use with testing. */
	static void checkForCycleAndRecover(
			Point fromImpl,
			Point toImpl) 
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
	static void recoverFromCycle(Point fromImpl, Point toImpl) {
		// Uh-oh, error, go into damage control.
		fromImpl.unAddEdge(toImpl);
	}
	
	/** Indicates that {@code interval} should execute while holding
	 *  an exclusive lock on {@code guard}.  The lock will be automatically
	 *  acquired sometime before {@code interval.start} and release
	 *  sometime after {@code interval.end}. */
	public static void exclusiveLock(Interval interval, Lock lock) {
		if(interval != null && lock != null) {
			Current current = Current.get();
			current.checkCanAddDep(interval.start);
			interval.addLock(lock, true);
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

	/** Waits for {@code ep} to complete and returns its result.
	 *  Resets the currentInterval afterwards. */
	static void join(Point pnt) {
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
	public static <R> R subinterval(final SubintervalTask<R> task) 
	{		
		// This could be made more optimized, but it will do for now:
		Current current = Current.get();
		
		SubintervalImpl<R> subinterval = current.start.insertSubintervalAfter(current.inter, task);			
		if(Debug.ENABLED)
			Debug.subInterval(subinterval, task.toString());		
		subinterval.start.occur();
		assert subinterval.start.didOccur();
		subinterval.exec();
		try {
			join(subinterval.end); // may throw an exception
			return subinterval.result;
		} finally {
			current.updateStart(subinterval.end);			
		}
	}		
	
	/**
	 * Variant of {@link #subinterval(SubintervalTask)} for
	 * subintervals that do not return a value. */
	public static void subinterval(final VoidSubinterval task)
	{
		subinterval(new SubintervalTask<Void>() {
			public Void run(Interval subinterval) {
				task.run(subinterval);
				return null;
			}
		});
	}

	/** 
	 * Returns the point which represents the end of the entire
	 * computation.  This point will not occur until all other
	 * points have occurred, and it is the only point without a bound. */
	public static Point rootEnd() {
		return rootEnd;
	}

}
