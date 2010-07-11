package ch.ethz.intervals.impl;

import static ch.ethz.intervals.util.ChunkList.NORMAL;
import static ch.ethz.intervals.util.ChunkList.SPECULATIVE;
import static ch.ethz.intervals.util.ChunkList.TEST_EDGE;
import static ch.ethz.intervals.util.ChunkList.WAITING;
import static ch.ethz.intervals.util.ChunkList.speculative;

import java.io.IOException;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;
import java.util.concurrent.atomic.AtomicIntegerFieldUpdater;

import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.Point;
import ch.ethz.intervals.impl.ThreadPool.Worker;
import ch.ethz.intervals.util.ChunkList;

import com.smallcultfollowing.lathos.Lathos;
import com.smallcultfollowing.lathos.Output;
import com.smallcultfollowing.lathos.Page;
import com.smallcultfollowing.lathos.PageContent;

public final class PointImpl 
implements Point, Page, RefManipulator
{
	public static final int NO_FLAGS = 0;
	public static final int FLAG_END = 1;		  /** is end point? */
	public static final int FLAG_INLINE = 2; /** is point of a sync. interval? */
	
	/** Value of {@link #waitCount} if we have occurred without any errors. */
	public static final int OCCURRED_WITHOUT_ERROR = -1;
	
	/** Value of {@link #waitCount} if we have occurred with uncaught exceptions. */
	public static final int OCCURRED_WITH_ERROR = -2;
	
	private static AtomicIntegerFieldUpdater<PointImpl> waitCountUpdater =
		AtomicIntegerFieldUpdater.newUpdater(PointImpl.class, "waitCount");

	final String name;							/** Possibly null */
	final PointImpl bound;						/** if a start point, the paired end point.  otherwise, null. */
	final int flags;                            /** various flags */
	final int depth;							/** depth of bound + 1 */
	
	private ChunkList<PointImpl> outEdges;      /** Linked list of outgoing edges from this point. */
	private volatile int waitCount;           	/** Number of preceding points that have not arrived. */	                                              	
	private IntervalImpl intervalImpl;          /** Interval which owns this point.  Set to {@code null} when the point occurs. */

	PointImpl(String name, int flags, PointImpl bound, int waitCount, IntervalImpl intervalImpl) {
		this.name = name;
		this.flags = flags;
		this.bound = bound;
		this.waitCount = waitCount;
		this.intervalImpl = intervalImpl;
		this.depth = (bound == null ? 0 : bound.depth + 1);
	}
	
	final boolean isStartPoint() {
		return (flags & FLAG_END) == 0;
	}
	
	final boolean isEndPoint() {
		return (flags & FLAG_END) == FLAG_END;
	}
	
	final boolean isInline() {
		return (flags & FLAG_INLINE) == FLAG_INLINE;
	}

	final boolean isAsync() {
		return !isInline();
	}

	final boolean isInlineEnd() {
		return (flags & (FLAG_INLINE|FLAG_END)) == (FLAG_INLINE|FLAG_END);
	}

	final synchronized ChunkList<PointImpl> outEdgesSync() {
		return outEdges;
	}
	
	final private boolean didOccur(int wc) {
		return wc < 0;
	}
	
	final boolean didOccur() {
		return didOccur(waitCount);
	}
	
	final boolean didOccurWithoutError() {
		return (waitCount == OCCURRED_WITHOUT_ERROR);
	}

	final boolean didOccurWithError() {
		return (waitCount == OCCURRED_WITH_ERROR);
	}
	
	final PointImpl interBound() {
		if(isStartPoint()) return bound.bound;
		return bound;
	}
	
	private void cancel() {
		assert !didOccur();
		intervalImpl.cancel(this);
	}

	/** 
	 * Returns the interval with which this point is associated or
	 * null if this point has occurred.  Note that returning non-null  
	 * is not a guarantee that point has not occurred by the time you
	 * receive the return value!
	 */
	IntervalImpl racyInterval() {
		return this.intervalImpl;
	}

	@Override
	public String toString() {
		if(name != null)
			if(isStartPoint())
				return name + ".start";
			else
				return name + ".end";
		return "Point("+System.identityHashCode(this)+")";
	}
	
	@Override
	public PointImpl mutualBound(Point _j) {
		PointImpl j = (PointImpl) _j;
		PointImpl i = this;
		
		while(i.depth > j.depth) {
			i = i.bound;		
		}
		
		while(j.depth > i.depth) {
			j = j.bound;
		}
		
		while(i != j) {
			i = i.bound;
			j = j.bound;
		}
		
		return i;
	}
	
	@Override public final PointImpl bound() {
		return bound;
	}
	
	public final boolean isBoundedBy(PointImpl p) {
		if(bound != null) {
			PointImpl b = bound;
			while(b.depth > p.depth) 
				b = b.bound;
			return (b == p);
		}
		return false;
	}
	
	/** True if {@code p} bounds {@code this} or one of its bounds */
	@Override public final boolean isBoundedBy(Point p) {
		if(p instanceof PointImpl) 
			return isBoundedBy((PointImpl)p);
		return false; // always bounded by a genuine Point, not some funky mirror
	}

	/** True if {@code p == this} or {@link #isBoundedBy(PointImpl)} */
	public final boolean isBoundedByOrEqualTo(PointImpl p) {
		return (this == p) || isBoundedBy(p);
	}

	/** Returns true if {@code this} <i>happens before</i> {@code p} */
	@Override public final boolean hb(final Point p) {
		return hb((PointImpl) p, SPECULATIVE|TEST_EDGE);
	}
	
	/** Returns true if {@code this == p} or {@code this} <i>happens before</i> {@code p} */
	@Override public boolean hbeq(final Point p) {
		return (this == p) || hb(p);
	}
	
	/** Returns true if {@code this} <i>happens before</i> {@code p},
	 *  including speculative edges. */
	boolean hbOrSpec(final PointImpl p) {
		return hb(p, TEST_EDGE);
	}
	
	/** true if {@code this} -> {@code p}.
	 * @param tar another point
	 * @param skipFlags Skips edges which have any of the flags in here. */
	private boolean hb(final PointImpl tar, final int skipFlags) {
		assert tar != null;
		
		// XXX We currently access the list of outgoing edges with
		// XXX outEdgesSync.  Is that necessary?  It seems like a
		// XXX volatile might be enough, at worst it would miss a path--
		// XXX Except that missing a path is bad when checking for cycles!
		// XXX Otherwise, since safety conditions are established
		// XXX by the PRESENCE of paths and not their absence, we would
		// XXX be able to remove synchronized checks if we could detect
		// XXX cycles another way, or at least synchronize in some other way
		// XXX when checking for cycles.  Perhaps a global clock.
		
		// Some simple checks:
		if(tar == this)
			return false;
		if(this.isBoundedBy(tar))
			return true;
		
		// If this is a start point, then it's connected to anything which is
		// bound by the corresponding end point.  This check is not merely 
		// an optimization: we have to do it because there are no links in the
		// data structures connecting a start to its children.
		if(isStartPoint() && tar.isBoundedBy(bound))
			return true;
		
		// Efficiently check whether a point bounds tar or not:
		class BoundHelper {
			final PointImpl[] tarBounds = tar.getBounds();
			
			boolean boundsTar(PointImpl p) {
				return p.depth < tarBounds.length && tarBounds[p.depth] == p;
			}
			
			// True if the user could legally invoke addHb(src, tar) 
			boolean userCouldLegallyAddHbToTarFrom(PointImpl src) {
				PointImpl srcInterBound = src.interBound();
				return srcInterBound == null || boundsTar(srcInterBound);
			}
			
			boolean isPrecedingInlineSibOfTar(PointImpl p) {
				// Check for this subtle scenario:
				//
				//       p           p.bound  
				// >-->--<-...........<
				//         :         :
				//         :   ...   :
				//         :         :
				//         :  >---<  :
				//         : tar     :
				//         +.........+
				//
				// Here, p is an inline interval.  Note that p.parent.end
				// does not HB tar, but p.end does, because inline
				// intervals always execute before their async. siblings.
				//
				// In fact, this same condition holds for inline subintervals
				// of p, but as they are bounded by p, they will eventually
				// reach this point.
				
				if(!p.isInlineEnd() || p.bound == null) 
					return false;
				
				if(p.bound == tar)
					return true;
				
				if(!boundsTar(p.bound))
					return false;
				
				int depthp1 = p.bound.depth + 1;
				return tarBounds[depthp1].isAsync();
			}
		}
		final BoundHelper bh = new BoundHelper();
		
		PointImpl src = this;
		
		// If src could not legally connect to tar, then it can only HB tar
		// if its bound HB tar:
		while(!bh.userCouldLegallyAddHbToTarFrom(src))
			src = src.interBound();
		
		// If src is asnyc w/ no outgoing edges, then it can only HB tar if its bound HB tar:
		//   (This is just a micro optimization to avoid doing BFS)
		while(src.outEdgesSync() == null && !src.isInline()) {
			src = src.bound;
			if(src == null) 
				return false;
		}
		
		// If src bounds tar, then tar hb src, so src cannot hb tar: 
		if(bh.boundsTar(src))
			return false;
		
		// At this point, we just have to bite the bullet and do a BFS to see
		// whether src can reach tar.  
		class SearchHelper {
			boolean foundIt = false;
			final Set<PointImpl> visited = new HashSet<PointImpl>(64);
			final LinkedList<PointImpl> queue = new LinkedList<PointImpl>();
			
			boolean shouldContinue() {
				return !foundIt && !queue.isEmpty();
			}
			
			boolean tryEnqueue(PointImpl pnt) {
				if(pnt != null) {		
					if(
							// found tar
							(pnt == tar)
							
							// found start point of an ancestor of tar
							|| (pnt.isStartPoint() && bh.boundsTar(pnt.bound))
					) {
						foundIt = true;
						return true; // found it
					}

					if(!bh.boundsTar(pnt) && visited.add(pnt))
						queue.add(pnt);
				}
				return false;
			}
		}
		final SearchHelper sh = new SearchHelper();
		sh.queue.add(src);
		
		do {
			PointImpl q = sh.queue.remove();
			
			assert !bh.boundsTar(q);
			if(bh.isPrecedingInlineSibOfTar(q)) 
				return true;
			
			if(sh.tryEnqueue(q.bound))
				return true;
			
			// Only explore non-bound edges if it's 
			// legal for the user to connect q to tar.
			if(bh.userCouldLegallyAddHbToTarFrom(q)) {
				ChunkList<PointImpl> qOutEdges = q.outEdgesSync(); 
				if(qOutEdges != null) {
					new ChunkList.InterruptibleIterator<PointImpl>(qOutEdges) {
						public boolean forEach(PointImpl toPoint, int flags) {
							if((flags & skipFlags) == 0)
								return sh.tryEnqueue(toPoint);
							return false;
						}
					};
				}
			}
		} while(sh.shouldContinue());
		
		return sh.foundIt;
	}

	/** When a preceding point (or something else we were waiting for)
	 *  occurs, this method is invoked.  Once {@link #waitCount} 
	 *  reaches 0, invokes {@link IntervalImpl#didReachWaitCountZero(PointImpl)}.
	 *  
	 *  @param cnt the number of preceding things that occurred,
	 *  should always be positive and non-zero */
	final void arrive(int cnt, RefManipulator from) {
		int newCount = waitCountUpdater.addAndGet(this, -cnt);
		
		if(Debug.ENABLED)
			Debug.debug.postDecRef(this, from, newCount);
		
		assert newCount >= 0;
		if(newCount == 0 && cnt != 0)
			didReachWaitCountZero();
	}

	/** Invoked by {@link #arrive(int)} when wait count reaches zero. */
	void didReachWaitCountZero() {
		intervalImpl.didReachWaitCountZero(this);
	}
	
	/** Invoked by {@link #intervalImpl} after wait count reaches zero.
	 *  Each point occurs precisely once.  
	 *  
	 *  @param withError true if there were uncaught exceptions that propagate to this point. */
	final void occur(boolean withError) {
		assert waitCount == 0;
		
		int newWaitCount = (withError ? OCCURRED_WITH_ERROR : OCCURRED_WITHOUT_ERROR);

		// Save copies of our outgoing edges at the time we occurred:
		//      They may be modified further while we are notifying successors.
		final ChunkList<PointImpl> outEdges;
		synchronized(this) {
			outEdges = this.outEdges;
			this.waitCount = newWaitCount;
			if(bound == null) // If this is (or could be) a root subinterval...
				notifyAll();  // ...someone might be wait()ing on us!
		}
		
		ExecutionLog.logArrive(this);
		
		if(Debug.ENABLED)
			Debug.debug.postOccur(this);
		
		// Notify our successors:
		if(outEdges != null) {
			new ChunkList.Iterator<PointImpl>(outEdges) {
				public void doForEach(PointImpl toPoint, int flags) {
					if(ChunkList.waiting(flags)) {
						notifySuccessor(toPoint, true);
					}
				}
			};
		}
		if(bound != null)
			notifySuccessor(bound, true);
		
		intervalImpl.didOccur(this);
		intervalImpl = null;
	}
	
	/** Takes the appropriate action to notify a successor {@code pnt}
	 *  that {@code this} has occurred.  Propagates exceptions and 
	 *  optionally invokes {@link #arrive(int)}. */
	private void notifySuccessor(PointImpl pnt, boolean arrive) {
		if(didOccurWithError()) {
			/* No need to deliver the error to our bound:
			 * - If we originated the error, it's a vertical error delivered by us.
			 * - If someone else with same bound originated error, it's a vertical error
			 *   delivered by them.
			 * - If someone else with a greater bound than us did so, it was done when
			 *   the error was delivered to us.
			 * But we do need to deliver to anyone up to but not including
			 * the bound.  See {@link TestErrorPropagation#predecessorsOfSubintervalEndThatDie()}
			 * for reason why. */
			PointImpl interBound = interBound();
			PointImpl p = pnt;
			while(p != interBound) {
				p.cancel();
				p = p.bound;
			}
		}
		if(arrive)
			pnt.arrive(1, this);
	}

	/** Adds to the wait count.  Only safe when the caller is
	 *  one of the people we are waiting for.  
	 *  <b>Does not acquire a lock on this.</b> */
	protected void addWaitCount(RefManipulator from) {
		int newCount = waitCountUpdater.incrementAndGet(this);
		assert newCount > 1;
		
		if(Debug.ENABLED)
			Debug.debug.postAddRef(this, from, newCount);
	}

	protected void addWaitCountUnsync(int cnt) {
		int newCount = waitCount + cnt;
		waitCount = newCount;
		assert newCount > 0;
	}
	
	/** Adds to the wait count but only if the point has
	 *  not yet occurred.  Always safe.
	 *  @return true if the add succeeded. */
	synchronized boolean tryAddWaitCount() {
		if(didOccur())
			return false;
		waitCount++;
		return true;
	}
	
	/**
	 * Waits until this point has occurred.  If this is a helper thread,
	 * tries to do useful work in the meantime!
	 */
	void join() {
		Worker worker = ContextImpl.POOL.currentWorker();
	
		if(worker == null) {
			synchronized(this) {
				while(!didOccur())
					try {
						wait();
					} catch (InterruptedException e) {
						throw new RuntimeException(e); // Should never happen
					}
			}
		} else {
			while(true) {
				int wc;
				synchronized(this) {
					wc = waitCount;
				}
				if(didOccur(wc))
					return;
				if(!worker.doWork(false))
					Thread.yield();				
			}
		}
	}

	/** Simply adds an outgoing edge, without acquiring locks or performing any
	 *  further checks. */
	private void primAddOutEdge(PointImpl targetPnt, int flags) {
		outEdges = ChunkList.add(outEdges, targetPnt, flags);
	}
	
	void addSpeculativeEdge(PointImpl targetPnt, int flags) {
		synchronized(this) {
			primAddOutEdge(targetPnt, flags | ChunkList.SPECULATIVE);
		}
	}
	
	/**
	 * Optimized routine for the case where 'this' is known to have
	 * already occurred and not to have had any exceptions.
	 */
	void addEdgeAfterOccurredWithoutException(PointImpl targetPnt, int edgeFlags) {
		synchronized(this) {
			assert didOccurWithoutError();			
			primAddOutEdge(targetPnt, edgeFlags);
		}
	}

	/**
	 * Adds an edge from {@code this} to {@code toImpl}, doing no safety
	 * checking, and adjusts the wait count of {@code toImpl}.
	 *  
	 * Returns the number of wait counts added to {@code toImpl}.
	 */
	void addEdgeAndAdjust(PointImpl toImpl, int flags) {
		assert !speculative(flags) : "addEdgeAndAdjust should not be used for spec. edges!";		
		
		// Note: we must increment the wait count before we release
		// the lock on this, because otherwise toImpl could arrive and
		// then decrement the wait count before we get a chance to increment
		// it.  Therefore, it's important that we do not have to acquire a lock
		// on toImpl, because otherwise deadlock could result.
		synchronized(this) {			
			if(!didOccur()) {
				primAddOutEdge(toImpl, flags | ChunkList.WAITING); 
				toImpl.addWaitCount(this);
				return;
			} else {
				primAddOutEdge(toImpl, flags);
			}
		}
		
		// If we already occurred, then we may still have to push the pending
		// exceptions, but we do not need to invoke arrive():
		assert didOccur();
		notifySuccessor(toImpl, false);
	}
	
	/** Removes an edge to {@code toImpl}, returning true 
	 *  if this point has occurred. */
	void unAddEdge(PointImpl toImpl) {
		synchronized(this) {
			ChunkList.remove(outEdges, toImpl);
		}
	}

	void confirmEdgeAndAdjust(PointImpl toImpl, int flags) {
		
		// Careful
		//
		// The edge to toImpl was speculative.  We are now going
		// to convert it into an ordinary edge.  If we are doing this
		// conversion before we occurred, then we will have to set the
		// WAITING flag on it and add to its wait count.
		//
		// Otherwise, we just leave its ref count alone, but we may have
		// to propagate pendingExceptions to it.
		
		synchronized(this) {
			if(!didOccur()) {
				toImpl.addWaitCount(this);
				ChunkList.removeSpeculativeFlagAndAdd(outEdges, toImpl, WAITING);
				return;
			} else {
				ChunkList.removeSpeculativeFlagAndAdd(outEdges, toImpl, 0);
			}			
		} 
		
		// If we get here, then we occurred either before the speculative
		// edge was added, or while it was being confirmed.  In that case, 
		// we have to notify it of any pending exceptions, but we do not
		// need to invoke accept():
		assert didOccur();	
		notifySuccessor(toImpl, false);
	}

	@Override
	public void addHb(Point _to) {
		PointImpl to = (PointImpl)_to;
		
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
		
		current.checkEdgeEndPointsProperlyBound(this, to);
		
		// Avoid edges that duplicate the bound.  Besides
		// saving space, this check lets us guarantee that
		// walking the outEdges from a point never leads to its
		// bound.  We rely on this in error propagation code,
		// since the bound is somewhat special.
		if(isBoundedBy(to))
			return; 
		
		// Optimistically add edge (though it may cause a cycle!):
		//
		//   If safety checks are enabled, then we initially record
		//   the edge as non-deterministic until we have confirmed
		//   that it causes no problems.
		
		if(!Intervals.SAFETY_CHECKS) {
			addEdgeAndAdjust(to, NORMAL);
		} else {
			// Really, these helper methods ought to be inlined,
			// but they are separated to aid in testing. 
			optimisticallyAddEdge(to);
			checkForCycleAndRecover(to);			
		}
		
		ExecutionLog.logEdge(this, to);
	}

	
	/** Helper method of {@link #addHb(Point)}.
	 *  Pulled apart for use with testing. */
	void optimisticallyAddEdge(PointImpl to) 
	{
		// Note: the edge is considered speculative until we have
		// verified that the resulting graph is acyclic.
		addSpeculativeEdge(to, NORMAL);
	}
	
	/** Helper method of {@link #addHb(Point)}.
	 *  Pulled apart for use with testing. */
	void checkForCycleAndRecover(PointImpl to) 
	{
		if(to.hbOrSpec(this)) {
			recoverFromCycle(to);
			throw new IntervalException.Cycle(this, to);
		} else {
			confirmEdgeAndAdjust(to, NORMAL);
		}
	}

	/** Helper method of {@link #addHb(Point)}.
	 *  Pulled apart for use with testing. */
	void recoverFromCycle(PointImpl to) {
		// Uh-oh, error, go into damage control.
		unAddEdge(to);
	}

	@Override
	public PointImpl[] getBounds() {
		PointImpl b = this;
		PointImpl[] bounds = new PointImpl[depth+1];
		for(int i = depth; i >= 0; i--) {
			bounds[i] = b;
			b = b.bound;
		}
		return bounds;			
	}
	
	// =====================================================================================
	// Lathos routines
	
	@Override
	public void renderInLine(Output output) throws IOException {
		Lathos.renderInLine(this, output);
	}

	@Override
	public void renderInPage(final Output out) throws IOException {
		out.startPage(this);
		
		out.startPar();
		out.startBold();
		out.outputText("Point: ");
		out.outputText(toString());
		out.endBold();
		out.endPar();
		
		int waitCount;
		ChunkList<PointImpl> outEdges;
		synchronized(this) {
			waitCount = waitCountUpdater.get(this);
			outEdges = this.outEdges;
		}

		// Fields:
		out.startTable();
		Lathos.row(out, "Name", name);
		Lathos.row(out, "Bound", bound);
		Lathos.row(out, "Depth", depth);
		Lathos.row(out, "End?", isEndPoint());
		Lathos.row(out, "Inline?", isInline());
		Lathos.row(out, "Wait count", waitCount);
		out.endTable();
		
		out.startPage(null);
		out.startBold();
		out.outputText("Edges");
		out.endBold();
		out.startTable();
		Lathos.headerRow(out, "toPoint", "speculative", "waiting", "test edge");		
		new ChunkList.Iterator<PointImpl>(outEdges) {
			@Override public void doForEach(PointImpl toPoint, int flags) {
				try {
					Lathos.row(out, 
							toPoint, 
							(flags & ChunkList.SPECULATIVE) != 0,
							(flags & ChunkList.WAITING) != 0,
							(flags & ChunkList.TEST_EDGE) != 0);
				} catch (IOException e) {
					throw new RuntimeException(e);
				}
			}
		};
		out.endTable();
		out.endPage(null);
		
		Debug.debug.renderEventsForObject(out, this);
		
		out.endPage(this);
	}
	
	@Override
	public Page getParent() {
		return null;
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
