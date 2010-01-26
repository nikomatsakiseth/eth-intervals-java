package ch.ethz.intervals;

import static ch.ethz.intervals.util.ChunkList.SPECULATIVE;
import static ch.ethz.intervals.util.ChunkList.TEST_EDGE;
import static ch.ethz.intervals.util.ChunkList.WAITING;
import static ch.ethz.intervals.util.ChunkList.speculative;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;
import java.util.concurrent.atomic.AtomicIntegerFieldUpdater;

import ch.ethz.intervals.ThreadPool.Worker;
import ch.ethz.intervals.mirror.PointMirror;
import ch.ethz.intervals.util.ChunkList;

public final class Point 
implements PointMirror 
{
	public static final int NO_FLAGS = 0;
	public static final int FLAG_END = 1;		  /** is end point? */
	public static final int FLAG_SYNCHRONOUS = 2; /** is point of a sync. interval? */
	
	public static final int FLAGS_SYNCHRONOUS_END = FLAG_END | FLAG_SYNCHRONOUS;	

	/** Value of {@link #waitCount} if we have occurred without any errors. */
	public static final int OCCURRED_WITHOUT_ERROR = -1;
	
	/** Value of {@link #waitCount} if we have occurred with uncaught exceptions. */
	public static final int OCCURRED_WITH_ERROR = -2;
	
	private static AtomicIntegerFieldUpdater<Point> waitCountUpdater =
		AtomicIntegerFieldUpdater.newUpdater(Point.class, "waitCount");

	final String name;							/** Possibly null */
	final Point bound;						  	/** if a start point, the paired end point.  otherwise, null. */
	final int flags;                            /** various flags */
	final int depth;							/** depth of bound + 1 */
	
	private ChunkList<Point> outEdges;          /** Linked list of outgoing edges from this point. */
	private volatile int waitCount;           	/** Number of preceding points that have not arrived. */	                                              	
	private Interval interval;            	  	/** Interval which owns this point.  Set to {@code null} when the point occurs. */

	Point(String name, int flags, Point bound, int waitCount, Interval interval) {
		this.name = name;
		this.flags = flags;
		this.bound = bound;
		this.waitCount = waitCount;
		this.interval = interval;
		this.depth = (bound == null ? 0 : bound.depth + 1);
	}
	
	final boolean isStartPoint() {
		return (flags & FLAG_END) == 0;
	}
	
	final boolean isEndPoint() {
		return (flags & FLAG_END) == FLAG_END;
	}
	
	final boolean isSynchronous() {
		return (flags & FLAG_SYNCHRONOUS) == FLAG_SYNCHRONOUS;
	}

	final synchronized ChunkList<Point> outEdgesSync() {
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

	private void cancel() {
		assert !didOccur();
		interval.cancel();
	}

	/** 
	 * Returns the interval with which this point is associated or
	 * null if this point has occurred.  Note that returning non-null  
	 * is not a guarantee that point has not occurred by the time you
	 * receive the return value!
	 */
	Interval racyInterval() {
		return this.interval;
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
	
	Point mutualBound(Point j) {
		
		Point i = this;
		
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
	
	@Override public final Point bound() {
		return bound;
	}
	
	public final boolean isBoundedBy(Point p) {
		if(bound != null) {
			Point b = bound;
			while(b.depth > p.depth) 
				b = b.bound;
			return (b == p);
		}
		return false;
	}
	
	/** True if {@code p} bounds {@code this} or one of its bounds */
	@Override public final boolean isBoundedBy(PointMirror p) {
		if(p instanceof Point) 
			return isBoundedBy((Point)p);
		return false; // always bounded by a genuine Point, not some funky mirror
	}

	/** True if {@code p == this} or {@link #isBoundedBy(Point)} */
	public final boolean isBoundedByOrEqualTo(Point p) {
		return (this == p) || isBoundedBy(p);
	}

	/** Returns true if {@code this} <i>happens before</i> {@code p} */
	@Override public final boolean hb(final PointMirror p) {
		return hb(p, SPECULATIVE|TEST_EDGE);
	}
	
	/** Returns true if {@code this == p} or {@code this} <i>happens before</i> {@code p} */
	@Override public boolean hbeq(final PointMirror p) {
		return (this == p) || hb(p);
	}
	
	/** Returns true if {@code this} <i>happens before</i> {@code p},
	 *  including speculative edges. */
	boolean hbOrSpec(final PointMirror p) {
		return hb(p, TEST_EDGE);
	}
	
	/** true if {@code this} -> {@code p}.
	 * @param tar another point
	 * @param skipFlags Skips edges which have any of the flags in here. */
	private boolean hb(final PointMirror tar, final int skipFlags) {
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
			final PointMirror[] tarBounds = Intervals.bounds(tar);
			
			boolean boundsTar(Point p) {
				return p.depth < tarBounds.length && tarBounds[p.depth] == p;
			}
			
			// True if the user could legally invoke addHb(src, tar) 
			boolean userCouldLegallyAddHbToTarFrom(Point src) {
				return src.bound == null || boundsTar(src.bound);
			}
		}
		final BoundHelper bh = new BoundHelper();
		
		Point src = this;
		
		// If src could not legally connect to tar, then it can only HB tar
		// if its bound HB tar:
		while(!bh.userCouldLegallyAddHbToTarFrom(src))
			src = src.bound;
		
		// If src has no outgoing edges, then it can only HB tar if its bound HB tar:
		while(src.outEdgesSync() == null) {
			src = src.bound;
			if(src == null) return false;
			if(bh.boundsTar(src)) return false;
		}
		
		// At this point, we just have to bite the bullet and do a BFS to see
		// whether src can reach tar.  
		class SearchHelper {
			boolean foundIt = false;
			final Set<Point> visited = new HashSet<Point>(64);
			final LinkedList<Point> queue = new LinkedList<Point>();
			
			boolean shouldContinue() {
				return !foundIt && !queue.isEmpty();
			}
			
			boolean tryEnqueue(Point pnt) {
				if(pnt != null) {		
					if((pnt == tar) // found tar
						|| 
						(pnt.isStartPoint() && bh.boundsTar(pnt.bound))) // found start point of an ancestor of p
					{
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
			Point q = sh.queue.remove();
			
			if(sh.tryEnqueue(q.bound))
				return true;
			
			// Only explore non-bound edges if it's 
			// legal for the user to connect q to tar.
			if(bh.userCouldLegallyAddHbToTarFrom(q)) {
				ChunkList<Point> qOutEdges = q.outEdgesSync(); 
				if(qOutEdges != null) {
					new ChunkList.InterruptibleIterator<Point>(qOutEdges) {
						public boolean forEach(Point toPoint, int flags) {
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
	 *  reaches 0, invokes {@link Interval#didReachWaitCountZero(Point)}.
	 *  
	 *  @param cnt the number of preceding things that occurred,
	 *  should always be positive and non-zero */
	final void arrive(int cnt) {
		int newCount = waitCountUpdater.addAndGet(this, -cnt);
		
		if(Debug.ENABLED) {
			Debug.arrive(this, cnt, newCount);
		}		
		
		assert newCount >= 0;
		if(newCount == 0 && cnt != 0)
			didReachWaitCountZero();
	}

	/** Invoked by {@link #arrive(int)} when wait count reaches zero,
	 *  but also from {@link Intervals#subinterval(SubintervalTask)} */
	void didReachWaitCountZero() {
		interval.didReachWaitCountZero(this);
	}
	
	/** Invoked by {@link #interval} after wait count reaches zero.
	 *  Each point occurs precisely once.  
	 *  
	 *  @param withError true if there were uncaught exceptions that propagate to this point. */
	final void occur(boolean withError) {
		assert waitCount == 0;
		
		int newWaitCount = (withError ? OCCURRED_WITH_ERROR : OCCURRED_WITHOUT_ERROR);

		// Save copies of our outgoing edges at the time we occurred:
		//      They may be modified further while we are notifying successors.
		final ChunkList<Point> outEdges;
		synchronized(this) {
			outEdges = this.outEdges;
			this.waitCount = newWaitCount;
			if(bound == null) // If this is (or could be) a root subinterval...
				notifyAll();  // ...someone might be wait()ing on us!
		}
		
		ExecutionLog.logArrive(this);
		
		if(Debug.ENABLED)
			Debug.occur(this, outEdges);
		
		// Notify our successors:
		if(outEdges != null) {
			new ChunkList.Iterator<Point>(outEdges) {
				public void doForEach(Point toPoint, int flags) {
					if(ChunkList.waiting(flags)) {
						notifySuccessor(toPoint, true);
					}
				}
			};
		}
		if(bound != null)
			notifySuccessor(bound, true);
		
		interval.didOccur(this);
		interval = null;
	}
	
	/** Takes the appropriate action to notify a successor {@code pnt}
	 *  that {@code this} has occurred.  Propagates exceptions and 
	 *  optionally invokes {@link #arrive(int)}. */
	private void notifySuccessor(Point pnt, boolean arrive) {
		if(pnt != bound && didOccurWithError())
			pnt.cancel();
		if(arrive)
			pnt.arrive(1);
	}

	/** Adds to the wait count.  Only safe when the caller is
	 *  one of the people we are waiting for.  
	 *  <b>Does not acquire a lock on this.</b> */
	protected void addWaitCount() {
		int newCount = waitCountUpdater.incrementAndGet(this);
		assert newCount > 1;
		if(Debug.ENABLED)
			Debug.addWaitCount(this, newCount);		
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
		Worker worker = Intervals.POOL.currentWorker();
	
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
	private void primAddOutEdge(Point targetPnt, int flags) {
		outEdges = ChunkList.add(outEdges, targetPnt, flags);
	}
	
	void addSpeculativeEdge(Point targetPnt, int flags) {
		synchronized(this) {
			primAddOutEdge(targetPnt, flags | ChunkList.SPECULATIVE);
		}
	}
	
	/**
	 * Optimized routine for the case where 'this' is known to have
	 * already occurred and not to have had any exceptions.
	 */
	void addEdgeAfterOccurredWithoutException(Point targetPnt, int edgeFlags) {
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
	void addEdgeAndAdjust(Point toImpl, int flags) {
		assert !speculative(flags) : "addEdgeAndAdjust should not be used for spec. edges!";		
		
		// Note: we must increment the wait count before we release
		// the lock on this, because otherwise toImpl could arrive and
		// then decrement the wait count before we get a chance to increment
		// it.  Therefore, it's important that we do not have to acquire a lock
		// on toImpl, because otherwise deadlock could result.
		synchronized(this) {			
			if(!didOccur()) {
				primAddOutEdge(toImpl, flags | ChunkList.WAITING); 
				toImpl.addWaitCount();
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
	void unAddEdge(Point toImpl) {
		synchronized(this) {
			ChunkList.remove(outEdges, toImpl);
		}
	}

	void confirmEdgeAndAdjust(Point toImpl, int flags) {
		
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
				toImpl.addWaitCount();
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
	public void addHb(PointMirror pnt) {
	}

}
