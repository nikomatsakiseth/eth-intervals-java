package ch.ethz.intervals;

import static ch.ethz.intervals.util.ChunkList.NORMAL;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.sun.istack.internal.NotNull;

import ch.ethz.intervals.guard.Guard;
import ch.ethz.intervals.mirror.PointMirror;

/** Static methods for creating and manipulating intervals. */
public class Intervals {
	
	/**
	 * If set to false, disables all safety checks against
	 * cycles or race conditions.  
	 */
	public static final boolean SAFETY_CHECKS = true;	

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
	
	public static @ParentForNew("") Dependency successor() {
		final Current current = Current.get();
		if(current.inter == null)
			throw new NotInRootIntervalException();
		return new Dependency() {			
			@Override
			public Interval parentForNewInterval() {
				return current.inter.parent;
			}
			
			@Override
			public void addHbToNewInterval(Interval inter) {	
				Intervals.addHb(current.inter.end, inter.start);
			}
		};		
	}
	
	/** Equivalent of {@code addHb(from.end, to.start)}
	 *  @see Intervals#start(Interval)
	 *  @see Intervals#end(Interval)
	 *  @see Intervals#addHb(Point, Point)
	 */
	public static void addHb(Interval from, Interval to) {
		addHb(from.end, to.start);
	}
	
	/** Equivalent of {@code addHb(from.end, to)}
	 *  @see Intervals#end(Interval)
	 *  @see Intervals#addHb(Point, Point)
	 */
	public static void addHb(Interval from, Point to) {
		addHb(from.end, to);
	}
	
	/** Equivalent of {@code addHb(from, to.start)}
	 *  @see Intervals#start(Interval)
	 *  @see Intervals#addHb(Point, Point)
	 */
	public static void addHb(Point from, Interval to) {
		addHb(from, to.start);
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
		
		current.checkEdgeEndPointsProperlyBound(from, to);
		
		// Avoid edges that duplicate the bound.  Besides
		// saving space, this check lets us guarantee that
		// walking the outEdges from a point never leads to its
		// bound.  We rely on this in error propagation code,
		// since the bound is somewhat special.
		if(from.isBoundedBy(to))
			return; 
		
		// Optimistically add edge (though it may cause a cycle!):
		//
		//   If safety checks are enabled, then we initially record
		//   the edge as non-deterministic until we have confirmed
		//   that it causes no problems.
		
		if(!SAFETY_CHECKS) {
			from.addEdgeAndAdjust(to, NORMAL);
		} else {
			// Really, these helper methods ought to be inlined,
			// but they are separated to aid in testing. 
			optimisticallyAddEdge(from, to);
			checkForCycleAndRecover(from, to);			
		}
		
		ExecutionLog.logEdge(from, to);
	}

	/** Helper method of {@link #addHb(Point, Point)}.
	 *  Pulled apart for use with testing. */
	static void optimisticallyAddEdge(
			Point from,
			Point to) 
	{
		// Note: the edge is considered speculative until we have
		// verified that the resulting graph is acyclic.
		from.addSpeculativeEdge(to, NORMAL);
	}
	
	/** Helper method of {@link #addHb(Point, Point)}.
	 *  Pulled apart for use with testing. */
	static void checkForCycleAndRecover(
			Point from,
			Point to) 
	{
		if(to.hbOrSpec(from)) {
			recoverFromCycle(from, to);
			throw new CycleException(from, to);
		} else {
			from.confirmEdgeAndAdjust(to, NORMAL);
		}
	}

	/** Helper method of {@link #addHb(Point, Point)}.
	 *  Pulled apart for use with testing. */
	static void recoverFromCycle(Point fromImpl, Point toImpl) {
		// Uh-oh, error, go into damage control.
		fromImpl.unAddEdge(toImpl);
	}
	
	/** Invokes {@link #addExclusiveLock(Interval, Lock, Guard)} with a 
	 *  {@code null} guard. */
	public static void addExclusiveLock(Interval interval, Lock lock) {
		addExclusiveLock(interval, lock, null);		
	}

	
	/** Indicates that {@code interval} should execute while holding
	 *  the exclusive lock {@code lock}.  The lock will be automatically
	 *  acquired sometime before {@code interval.start} and release
	 *  sometime after {@code interval.end}.  You may also, optionally, 
	 *  provide a guard {@code guard} which is being protected by this lock.
	 *   
	 *  @param interval the interval which should acquire the lock
	 *  @param lock the lock to be acquired
	 *  @param guard the guard whose data is being protected, or {@code null} */
	public static void addExclusiveLock(Interval interval, Lock lock, Guard guard) {
		if(interval != null && lock != null) {
			Current current = Current.get();
			current.checkCanAddDep(interval.start);
			interval.addExclusiveLock(lock, guard);
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
	 * Creates a new interval which executes during the current interval.
	 * This interval will execute {@code task}.  This function does not
	 * return until the new interval has completed.
	 * 
	 * <b>Note:</b> Exceptions that occur in {@code task} are 
	 * wrapped in {@link RethrownException} and rethrown immediately.
	 * Exceptions never propagate to the current interval.
	 */
	public static <R> R inline(final InlineTask<R> task) 
	{		
		// This could be made more optimized, but it will do for now:
		Current current = Current.get();
		
		String name = task.toString(); // WARNING: user code may throw an exception
		
		InlineIntervalImpl<R> subinterval = new InlineIntervalImpl<R>(name, current, task);
		
		if(current.mr != null && current.mr != current.start())
			current.mr.addEdgeAfterOccurredWithoutException(subinterval.start, NORMAL);
		
		if(Debug.ENABLED)
			Debug.subInterval(subinterval, task.toString());
		
		// Invoke the init function.
		try {
			task.init(subinterval); // user code could throw an exc
		} catch (Throwable thr) {
			subinterval.addVertExceptionUnsyc(thr);
		}
		
		// Schedule subinterval and wait for it to complete:
		subinterval.schedule();		
		subinterval.end.join();
		current.updateMostRecent(subinterval.end);
		return subinterval.readResultOrRethrowErrors();
	}		
	
	/**
	 * Variant of {@link #inline(InlineTask)} for
	 * subintervals that do not return a value. */
	public static void inline(final VoidInlineTask task)
	{
		inline(new InlineTask<Void>() {			
			@Override public String toString() {
				return task.toString();
			}			
			@Override public void init(Interval subinterval) {
				task.init(subinterval);
			}
			@Override public Void run(Interval subinterval) {
				task.run(subinterval);
				return null;
			}
		});
	}

	
	/** 
	 * Returns the point which represents the end of the entire
	 * computation.  This point will not occur until all other
	 * points have occurred, and it is the only point without a bound. */
	public static Dependency root() {
		return new Dependency() {			
			@Override
			public Interval parentForNewInterval() {
				return null;
			}
			
			@Override
			public void addHbToNewInterval(Interval inter) {
			}
		};
	}

	/** Returns an array {@code bounds} where {@code bounds[0]} == the bound at depth 0,
	 *  {@code bounds[depth] == this} */
	public static PointMirror[] bounds(PointMirror pointMirror) {
		if(pointMirror instanceof Point) {
			Point b = (Point)pointMirror;
			Point[] bounds = new Point[b.depth+1];
			for(int i = b.depth; i >= 0; i--) {
				bounds[i] = b;
				b = b.bound;
			}
			return bounds;			
		} else {
			List<PointMirror> lst = new ArrayList<PointMirror>();
			PointMirror bnd = pointMirror;
			while(bnd != null) {
				lst.add(bnd);
				bnd = bnd.bound();
			}
			Collections.reverse(lst);
			return lst.toArray(new PointMirror[lst.size()]);
		}
	}
	
	public static PointMirror mutualBound(PointMirror one, PointMirror two) {
		if(one instanceof Point && two instanceof Point) {
			return ((Point)one).mutualBound((Point) two);
		} else {
			PointMirror oneBounds[] = bounds(one);
			
			PointMirror b = two;
			while(b != null) {
				for(int i = 0; i < oneBounds.length; i++) 
					if(oneBounds[i] == b)
						return b;
				b = b.bound();
			}
			
			return null;
		}
	}

	/**
	 * Convenience method for asserting that the current interval is readable.
	 * Intended to be used like: {@code assert checkReadable(guard);}
	 * 
	 * @param guard the guard to check for readability
	 * @returns true if {@code guard} is readable, and throws an exception otherwise.
	 */
	public static boolean checkReadable(Guard guard) {
		Current current = Current.get();
		if(current.inter == null)
			throw new NotInRootIntervalException(); // later we could allow this maybe
		RuntimeException err = guard.checkReadable(current.mr, current.inter);
		if(err != null) throw err;
		return true;
	}
	
	/**
	 * Convenience method for asserting that the current interval is writable.
	 * 
	 * @see #checkReadable(Guard)
	 */
	public static boolean checkWritable(Guard guard) {
		Current current = Current.get();
		if(current.inter == null)
			throw new NotInRootIntervalException(); // later we could allow this maybe
		RuntimeException err = guard.checkWritable(current.mr, current.inter);
		if(err != null) throw err;
		return true;
	}
	
}
