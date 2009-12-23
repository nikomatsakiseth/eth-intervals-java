package ch.ethz.intervals;

import static ch.ethz.intervals.EdgeList.NONDETERMINISTIC;
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

final public class Point implements Dependency {
	
	public static final int OCCURRED = -1;	
	
	public static final int initialWaitCount = Integer.MAX_VALUE;
	
	private static AtomicIntegerFieldUpdater<Point> waitCountUpdater =
		AtomicIntegerFieldUpdater.newUpdater(Point.class, "waitCount");

	/** If this flag is set, then uncaught exceptions for this 
	 *  interval are NOT propagated to its parent. */
	protected static final int FLAG_MASK_EXC = 1;
	
	public final Point bound;                 /** Bound of this point (this->bound). */
	private final int depth;                  /** Depth in point bound tree. */
	private EdgeList outEdges;                /** Linked list of outgoing edges from this point. */
	private volatile int waitCount;           /** Number of preceding points that have not arrived.  
	                                              Set to {@link #OCCURRED} when this has occurred. 
	                                              Modified only through {@link #waitCountUpdater}. */
	private Set<Throwable> pendingExceptions; /** Exception(s) that occurred while executing the task or in some preceding point. */
	private int flags;                        /** Flags (see integer constants like {@link #FLAG_MASK_EXC}) */
	private LockList pendingLocks;            /** Locks to acquire once waitCount reaches 0 but before we occur */
	private Interval workItem;            /** Work to schedule when we occur (if any) */
	private Current unscheduled;

	/** Used only for the end point of the root interval. */
	Point(int waitCount) {
		bound = null;
		depth = 0;
		this.waitCount = waitCount;
	}

	/** Used only almost all points. */
	Point(Current unscheduled, Point bound, int waitCount, Interval workItem) {
		this.bound = bound;
		this.depth = bound.depth + 1;
		this.waitCount = waitCount;
		this.unscheduled = unscheduled;
		this.workItem = workItem;
	}
	
	boolean isUnscheduled(Current current) {
		return unscheduled == current;
	}
	
	void clearUnscheduled() {
		unscheduled = null;
	}
	
	synchronized EdgeList outEdgesSync() {
		return outEdges;
	}

	private boolean didOccur() {
		return waitCount == OCCURRED;
	}
	
	@Override
	public String toString() {
		return "Point("+System.identityHashCode(this)+")";
	}
	
	public final Point bound() {
		return bound;
	}

	/** True if {@code p} bounds {@code this} or one of its bounds */
	public boolean isBoundedBy(Point p) {
		Point pp = (Point) p;
		Point bb = bound;
		
		if(bb != null) {
			while(bb.depth > pp.depth)
				bb = bb.bound;
			
			return (pp == bb);
		} else {
			return false;
		}
	}

	/** Returns the mutual bound of {@code this} and {@code j} */
	public Point mutualBound(Point j) {
		Point i = this;
		
		// Move either i or j up the street so that
		// both pointers are at the same depth.
		int d = i.depth - j.depth;
		if(d > 0) // i.depth > j.depth
			while(d-- > 0)
				i = i.bound;
		else      // j.depth > i.depth
			while(d++ < 0)
				j = j.bound;
		
		// Move up in pairs till we find a common ancestor.
		while(i != j) {
			i = i.bound;
			j = j.bound;
		}
		
		return i;
	}
	
	public boolean hb(final Point p) {
		return hb((Point) p, NONDETERMINISTIC | SPECULATIVE);
	}
	
	/** true if {@code this} -> {@code p}.
	 * @param p another point
	 * @param skipFlags Skips edges which have any of the flags in here. */
	boolean hb(final Point p, final int skipFlags) {
		assert p != null;
		
		if(p == this)
			return false;
		if(isBoundedBy(p))
			return true;
		
		// Common case check:
		//
		//                 bound
		//                   /
		//   p --> this--bound
		//
		// Check for the scenario where 
		// (a) this is a start point;
		// (b) p shares a bound with our end point;
		// (c) this has no other successors.
		if(bound == null || (
				outEdges == null && 
				bound.outEdges == null &&
				bound.bound != null &&
				p.isBoundedBy(bound.bound)))
			return false;

		// Perform a BFS to find p:		
		final Point pBounds[] = p.bounds();
		final Queue<Point> queue = new LinkedList<Point>();
		final Set<Point> visited = new HashSet<Point>(64);
		queue.add(this);
		
		class Helper {
			boolean foundIt = false;
			
			boolean pIsBoundedBy(Point q) {
				return(q.depth < pBounds.length && pBounds[q.depth] == q);
			}
			
			/** Add {@code q} to queue if not already visited, returning
			 *  {@code true} if {@code q} is the node we are looking for. */
			boolean tryEnqueue(Point q) {
				assert q != null;
				
				if(q == p) {
					foundIt = true;
					return true; // found it
				}

				// If p is bounded by q, then p->q, so q cannot hb p.
				//
				// (If the user attempted to add such an edge, a cycle
				//  exception would result.)
				if(!pIsBoundedBy(q) && visited.add(q))
					queue.add(q);
				return false;
			}
		}
		final Helper h = new Helper();
		
		do {
			Point q = queue.remove();
			
			if(q.bound != null && h.tryEnqueue(q.bound))				
				return true;
			
			// Do we need to use synchronized accessor here?
			// Would it be good enough to use a volatile field for outEdges?
			new EdgeList.InterruptibleIterator(q.outEdgesSync()) {
				public boolean forEach(Point toPoint, int flags) {
					if((flags & skipFlags) == 0) {
						return h.tryEnqueue(toPoint);
					}
					return false;
				}
			};
		} while(!queue.isEmpty() && !h.foundIt);
		
		return h.foundIt;
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
			if(pendingLocks != null)
				acquirePendingLocks();
			else
				occur();
	}
	
	/** Invoked when the wait count is zero.  Attempts to
	 *  acquire all pending locks. */
	protected final void acquirePendingLocks() {
		waitCount = 1; // Wait for us to finish iterating through the list of locks.
			
		for(LockList lock = this.pendingLocks; lock != null; lock = lock.next)
			lock.lock.addExclusive(this);
		this.pendingLocks = null;
		
		arrive(1);
	}
	
	/** Invoked when the wait count is zero and all pending locks
	 *  are acquired. Each point occurs precisely once. */
	protected final void occur() {
		assert pendingLocks == null;
		assert waitCount == 0;
		assert unscheduled == null;
		
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
		notifySuccessor(bound, true);
		
		if(workItem != null) {
			if(pendingExceptions == null) {
				Intervals.POOL.submit(workItem);
				workItem = null;
			} else {
				bound.arrive(1);
			}
		}
	}
	
	/** Takes the appropriate action to notify a successor {@code pnt}
	 *  that {@code this} has occurred.  Propagates exceptions and 
	 *  optionally invokes {@link #arrive(int)}. */
	private void notifySuccessor(Point pnt, boolean arrive) {
		if((flags & FLAG_MASK_EXC) == 0 && pendingExceptions != null)
			pnt.addPendingExceptions(pendingExceptions);
		if(arrive)
			pnt.arrive(1);
	}

	/** Adds to the wait count.  Only safe when the caller is
	 *  one of the people we are waiting for.  
	 *  <b>Does not acquire a lock on this.</b> */
	protected void addWaitCount() {
		int newCount = waitCountUpdater.incrementAndGet(this);
		if(Debug.ENABLED)
			Debug.addWaitCount(this, newCount);		
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
	
	synchronized void addPendingException(Throwable thr) {
		assert !didOccur() : "Cannot add a pending exception after pnt occurs!";
		if(bound == null) {
			thr.printStackTrace();
		} else {
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
	}


	/** Invoked when a child interval has thrown the exception 't'. 
	 *  Overwrites the field {@link #pendingExceptions} with {@code t}.
	 *  This is non-ideal, as it may result in exceptions being lost,
	 *  and it is also unpredictable which exception will be reported
	 *  (the one from the task? the one from a child? if so, which child)
	 *  The correct behavior is probably to construct an aggregate object
	 *  containing a set of all exceptions thrown.
	 */
	synchronized void addPendingExceptions(Set<Throwable> thr) {
		for(Throwable t : thr)
			addPendingException(t);
	}

	protected void addFlagBeforeScheduling(int flag) {
		this.flags = this.flags | flag;
	}

	protected void removeFlagBeforeScheduling(int flag) {
		this.flags = this.flags & (~flag);
	}

	void addPendingLock(Lock lock, boolean exclusive) {
		LockList list = new LockList(lock, exclusive, null);
		synchronized(this) {
			list.next = pendingLocks;
			pendingLocks = list;
		}
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
	void addEdgeAfterOccurredWithoutException(Point targetPnt, int flags) {
		synchronized(this) {
			assert didOccur() && pendingExceptions == null;
			primAddOutEdge(targetPnt, flags);
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
		// The edge to toImpl WAS speculative.  We are now going
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
	
	Point[] bounds() {
		Point[] result = new Point[depth];
		for(Point b = bound; b != null; b = b.bound)
			result[b.depth] = b;
		return result;
	}

	@Override
	public void addHbToNewInterval(Interval inter) {
	}

	@Override
	public Point boundForNewInterval() {
		return this;
	}
	
}
