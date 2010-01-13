package ch.ethz.intervals;

import static ch.ethz.intervals.EdgeList.SPECULATIVE;
import static ch.ethz.intervals.EdgeList.WAITING;
import static ch.ethz.intervals.EdgeList.speculative;

import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.atomic.AtomicIntegerFieldUpdater;

import ch.ethz.intervals.ThreadPool.Worker;

public abstract class Point {
	
	public static final int OCCURRED = -1;		/** Value of {@link #waitCount} once we have occurred */
	
	private static AtomicIntegerFieldUpdater<Point> waitCountUpdater =
		AtomicIntegerFieldUpdater.newUpdater(Point.class, "waitCount");

	final String name;							/** Possibly null */
	final Line line;							/** Line on which the point resides. */	
	final Point bound;						  	/** if a start point, the paired end point.  otherwise, null. */
	final int depth;							/** depth of bound + 1 */
	
	private EdgeList outEdges;                	/** Linked list of outgoing edges from this point. */
	private volatile int waitCount;           	/** Number of preceding points that have not arrived.  
	                                              	Set to {@link #OCCURRED} when this has occurred. 
	                                              	Modified only through {@link #waitCountUpdater}. */
	private Set<Throwable> pendingExceptions; 	/** Exception(s) that occurred while executing the task or in some preceding point. */
	private Interval interval;            	  	/** Interval which owns this point.  Set to {@code null} when the point occurs. */

	Point(String name, Line line, Point bound, int waitCount, Interval interval) {
		assert line != null && interval != null;
		this.name = name;
		this.line = line;
		this.bound = bound;
		this.waitCount = waitCount;
		this.interval = interval;
		this.depth = (bound == null ? 0 : bound.depth + 1);
	}
	
	abstract boolean isStartPoint();
	
	final boolean isEndPoint() {
		return !isStartPoint();
	}
	
	final boolean maskExceptions() {
		// The end of any subinterval masks exceptions
		if(!isEndPoint()) return false;
		
		if(bound == null)
			return (line == Line.rootLine);
		
		return (bound.line == line);
	}
	
	final synchronized EdgeList outEdgesSync() {
		return outEdges;
	}

	final boolean didOccur() {
		return waitCount == OCCURRED;
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
	
	/** True if {@code p} bounds {@code this} or one of its bounds */
	public boolean isBoundedBy(Point p) {
		if(depth > p.depth) {
			for(Point b = bound; b != null; b = b.bound)
				if(b == p)
					return true;
		}
		return false;
	}

	/** True if {@code p == this} or {@link #isBoundedBy(Point)} */
	public boolean isBoundedByOrEqualTo(Point p) {
		return (this == p) || isBoundedBy(p);
	}

	/** Returns true if {@code this} <i>happens before</i> {@code p} */
	public boolean hb(final Point p) {
		return hb(p, SPECULATIVE);
	}
	
	/** Returns true if {@code this == p} or {@code this} <i>happens before</i> {@code p} */
	public boolean hbeq(final Point p) {
		return (this == p) || hb(p);
	}
	
	boolean hbeq(Point p, int skipFlags) {
		return (this == p) || hb(p, skipFlags);
	}
	
	/** true if {@code this} -> {@code p}.
	 * @param tar another point
	 * @param skipFlags Skips edges which have any of the flags in here. */
	boolean hb(final Point tar, final int skipFlags) {
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
			final Point[] tarBounds = tar.bounds();
			
			boolean boundsTar(Point p) {
				return p.depth < tarBounds.length && tarBounds[p.depth] == p;
			}
			
			boolean couldLegallyBeConnectedToTar(Point src) {
				return src.bound == null || boundsTar(src.bound);
			}
		}
		final BoundHelper bh = new BoundHelper();
		
		Point src = this;
		
		// If src could not legally connect to tar, then it can only HB tar
		// if its bound HB tar:
		while(!bh.couldLegallyBeConnectedToTar(src))
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
			
			new EdgeList.InterruptibleIterator(q.outEdgesSync()) {
				public boolean forEach(Point toPoint, int flags) {
					if((flags & skipFlags) == 0) {
						return sh.tryEnqueue(toPoint);
					}
					return false;
				}
			};
		} while(sh.shouldContinue());
		
		return sh.foundIt;
	}

	/** When a preceding point (or something else we were waiting for)
	 *  occurs, this method is invoked. 
	 *  @param cnt the number of preceding things that occurred,
	 *  should always be positive and non-zero */
	protected final void arrive(int cnt) {
		int newCount = waitCountUpdater.addAndGet(this, -cnt);
		
		if(Debug.ENABLED) {
			Debug.arrive(this, cnt, newCount);
		}		
		
		assert newCount >= 0;
		if(newCount == 0 && cnt != 0)
			if(interval.willOccur(this, (pendingExceptions != null)))
				occur();
	}
	
	/** Invoked when the wait count is zero and all pending locks
	 *  are acquired. Each point occurs precisely once. */
	final void occur() {
		assert waitCount == 0;
		assert line.isScheduled();
		
		// Save copies of our outgoing edges at the time we occurred:
		//      They may be modified further while we are notifying successors.
		final EdgeList outEdges;
		synchronized(this) {
			outEdges = this.outEdges;
			this.waitCount = OCCURRED;
			notifyAll(); // in case anyone is joining us
		}
		
		ExecutionLog.logArrive(this);
		
		if(Debug.ENABLED)
			Debug.occur(this, outEdges);
		
		// Notify our successors:
		new EdgeList.Iterator(outEdges) {
			public void doForEach(Point toPoint, int flags) {
				if(EdgeList.waiting(flags))
					notifySuccessor(toPoint, true);
			}
		};
		if(bound != null)
			notifySuccessor(bound, true);
		else
			notifyRootEnd();
		
		interval.didOccur(this, (pendingExceptions != null));
		interval = null;
	}
	
	private void notifyRootEnd() {
		// what should we do if an exception is never caught?
		if(pendingExceptions != null && !maskExceptions()) {
			for(Throwable t : pendingExceptions)
				t.printStackTrace();
		}
	}
	
	/** Takes the appropriate action to notify a successor {@code pnt}
	 *  that {@code this} has occurred.  Propagates exceptions and 
	 *  optionally invokes {@link #arrive(int)}. */
	private void notifySuccessor(Point pnt, boolean arrive) {
		if(!maskExceptions() && pendingExceptions != null)
			pnt.addPendingExceptions(pendingExceptions);
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
				if(wc == OCCURRED)
					return;
				if(!worker.doWork(false))
					Thread.yield();				
			}
		}
	}

	/** Checks if a pending throwable is stored and throws a
	 *  {@link RethrownException} if so. */
	void checkAndRethrowPendingException() {
		if(pendingExceptions != null)
			throw new RethrownException(pendingExceptions);				
	}
	
	/** Adds {@code thr} to {@link #pendingExceptions} */
	synchronized void addPendingException(Throwable thr) {
		assert !didOccur() : "Cannot add a pending exception after pnt occurs!";
		
		// Using a PSet<> would really be better here:
		if(pendingExceptions == null)
			pendingExceptions = Collections.singleton(thr);
		else if(pendingExceptions.size() == 1) {
			pendingExceptions = new HashSet<Throwable>(pendingExceptions);
			pendingExceptions.add(thr);
		} else {
			pendingExceptions.add(thr);
		}
	}


	/** Invoked when a child interval has thrown the exceptions {@code thr}. */
	synchronized void addPendingExceptions(Set<Throwable> thr) {
		for(Throwable t : thr)
			addPendingException(t);
	}

	/** Simply adds an outgoing edge, without acquiring locks or performing any
	 *  further checks. */
	private void primAddOutEdge(Point targetPnt, int flags) {
		outEdges = EdgeList.add(outEdges, targetPnt, flags);
	}
	
	void addSpeculativeEdge(Point targetPnt, int flags) {
		synchronized(this) {
			primAddOutEdge(targetPnt, flags | EdgeList.SPECULATIVE);
		}
	}
	
	/**
	 * Optimized routine for the case where 'this' is known to have
	 * already occurred and not to have had any exceptions.
	 */
	void addEdgeAfterOccurredWithoutException(Point targetPnt, int edgeFlags) {
		synchronized(this) {
			assert didOccur();
			
			// In some cases, pendingExceptions may be non-null if this was a 
			// subinterval which rethrew the exceptions and had them caught.
			assert maskExceptions() || pendingExceptions == null;
			
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
		addEdgeAndAdjustDuringTest(toImpl, flags);
	}

	/**
	 * Like {@link #addEdgeAndAdjust(Point, int)} but skips some assertions
	 * that apply in normal code but not in testing code.
	 */
	void addEdgeAndAdjustDuringTest(Point toImpl, int flags) {
		// Note: we must increment the wait count before we release
		// the lock on this, because otherwise toImpl could arrive and
		// then decrement the wait count before we get a chance to increment
		// it.  Therefore, it's important that we do not have to acquire a lock
		// on toImpl, because otherwise deadlock could result.
		synchronized(this) {			
			if(!didOccur()) {
				primAddOutEdge(toImpl, flags | EdgeList.WAITING); 
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
			EdgeList.remove(outEdges, toImpl);
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
				EdgeList.removeSpeculativeFlagAndAdd(outEdges, toImpl, WAITING);
				return;
			} else {
				EdgeList.removeSpeculativeFlagAndAdd(outEdges, toImpl, 0);
			}			
		} 
		
		// If we get here, then we occurred either before the speculative
		// edge was added, or while it was being confirmed.  In that case, 
		// we have to notify it of any pending exceptions, but we do not
		// need to invoke accept():
		assert didOccur();	
		notifySuccessor(toImpl, false);
	}

	/** Returns an array {@code bounds} where {@code bounds[0]} == the bound at depth 0,
	 *  {@code bounds[depth] == this} */
	Point[] bounds() {
		Point[] bounds = new Point[depth+1];
		Point b = this;
		for(int i = depth; i >= 0; i--) {
			bounds[i] = b;
			b = b.bound;
		}
		return bounds;
	}

	int numPendingExceptions() {
		if(pendingExceptions == null)
			return 0;
		return pendingExceptions.size();
	}

}
