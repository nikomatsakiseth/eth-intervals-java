package ch.ethz.intervals;

import static ch.ethz.intervals.EdgeList.NONDETERMINISTIC;
import static ch.ethz.intervals.EdgeList.SPECULATIVE;
import static ch.ethz.intervals.EdgeList.speculative;

import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.atomic.AtomicIntegerFieldUpdater;

import com.sun.tools.jdi.LinkedHashMap;

import ch.ethz.intervals.ThreadPool.Worker;

class PointImpl implements Point {
	
	public static final int OCCURRED1 = -1;	
	public static final int OCCURRED2 = -2;	
	
	public static final int initialWaitCount = Integer.MAX_VALUE;
	
	private static AtomicIntegerFieldUpdater<PointImpl> waitCountUpdater =
		AtomicIntegerFieldUpdater.newUpdater(PointImpl.class, "waitCount");

	/** If this flag is set, then uncaught exceptions for this 
	 *  interval are NOT propagated to its parent. */
	protected static final int FLAG_MASK_EXC = 1;
	
	final PointImpl bound;                    /** Bound of this point (this->bound). */
	final int depth;                          /** Depth in point bound tree. */
	private EdgeList outEdges;                /** Linked list of outgoing edges from this point. */
	private volatile int waitCount;           /** Number of preceding points that have not arrived.  
	                                              Set to {@link #OCCURRED} when this has occurred. 
	                                              Modified only through {@link #waitCountUpdater}. */
	private Set<Throwable> pendingExceptions; /** Exception(s) that occurred while executing the task or in some preceding point. */
	private int flags;                        /** Flags (see integer constants like {@link #FLAG_MASK_EXC}) */
	private LockList pendingLocks;            /** Locks to acquire once waitCount reaches 0 but before we occur */
	private IntervalImpl workItem;            /** Work to schedule when we occur (if any) */
	private Current unscheduled;

	/** Used only for the end point of the root interval. */
	PointImpl(int waitCount) {
		bound = null;
		depth = 0;
		this.waitCount = waitCount;
	}

	/** Used only almost all points. */
	PointImpl(Current unscheduled, PointImpl bound, int waitCount) {
		this.bound = bound;
		this.depth = bound.depth + 1;
		this.waitCount = waitCount;
		this.unscheduled = unscheduled;
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
		return waitCount <= OCCURRED1;
	}
	
	@Override
	public String toString() {
		return "Point("+System.identityHashCode(this)+")";
	}

	@Override
	public Point bound() {
		return bound;
	}

	@Override
	public boolean isBoundedBy(Point p) {
		PointImpl pp = (PointImpl) p;
		PointImpl bb = bound;
		
		if(bb != null) {
			while(bb.depth > pp.depth)
				bb = bb.bound;
			
			return (pp == bb);
		} else {
			return false;
		}
	}

	/** Returns the mutual bound of {@code this} and {@code j} */
	public PointImpl mutualBound(PointImpl j) {
		PointImpl i = this;
		
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
	
	@Override
	public boolean hb(final Point p) {
		return hb((PointImpl) p, NONDETERMINISTIC | SPECULATIVE);
	}
	
	/** true if {@code this} -> {@code p}.
	 * @param p another point
	 * @param skipFlags Skips edges which have any of the flags in here. */
	boolean hb(final PointImpl p, final int skipFlags) {
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
		final PointImpl pBounds[] = p.bounds();
		final Queue<PointImpl> queue = new LinkedList<PointImpl>();
		final Set<PointImpl> visited = new HashSet<PointImpl>(64);
		queue.add(this);
		
		class Helper {
			boolean foundIt = false;
			
			boolean pIsBoundedBy(PointImpl q) {
				return(q.depth < pBounds.length && pBounds[q.depth] == q);
			}
			
			/** Add {@code q} to queue if not already visited, returning
			 *  {@code true} if {@code q} is the node we are looking for. */
			boolean tryEnqueue(PointImpl q) {
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
			PointImpl q = queue.remove();
			
			if(q.bound != null && h.tryEnqueue(q.bound))				
				return true;
			
			// Do we need to use synchronized accessor here?
			// Would it be good enough to use a volatile field for outEdges?
			new EdgeList.InterruptibleIterator(q.outEdgesSync()) {
				public boolean forEach(PointImpl toPoint, int flags) {
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
		// Temporarily make this large so it doesn't drop to zero while we work.
		// Note that everyone who might adjust this count has already arrived.
		// (Though as we acquire locks we will add new people)
		waitCount = initialWaitCount; 
			
		int waits = 0;
		for(LockList lock = this.pendingLocks; lock != null; lock = lock.next) {
			if(lock.exclusive)
				waits += lock.guard.addExclusive(this);
			else
				lock.guard.addShared(this);
		}				
		this.pendingLocks = null;
		
		arrive(initialWaitCount - waits); // may cause this method to be invoked recursively		
	}
	
	/** Invoked when the wait count is zero and all pending locks
	 *  are acquired. Each point occurs precisely once. */
	protected final void occur() {
		assert pendingLocks == null;
		assert waitCount == 0;
		assert unscheduled == null;
		
		final EdgeList outEdges;
		final int chunkMask;
		synchronized(this) {
			outEdges = this.outEdges;
			chunkMask = EdgeList.chunkMask(outEdges);
			this.waitCount = OCCURRED1;
			notifyAll(); // in case anyone is joining us
		}
		
		ExecutionLog.logArrive(this);
		
		if(Debug.ENABLED)
			Debug.occur(this, outEdges, chunkMask);
		
		// Propagate any exception to all successors unless we are
		// set to mask exceptions, or the successor is speculative:
		final Set<Throwable> pendingExceptions = this.pendingExceptions;
		if((flags & FLAG_MASK_EXC) == 0 && pendingExceptions != null) {
			new EdgeList.Iterator(outEdges, chunkMask) {
				public void doForEach(PointImpl toPoint, int flags) {
					if(!EdgeList.speculative(flags))
						toPoint.addPendingExceptions(pendingExceptions);
				}
			};
			bound.addPendingExceptions(pendingExceptions);
		}
		
		new EdgeList.Iterator(outEdges, chunkMask) {
			public void doForEach(PointImpl toPoint, int flags) {
				if(!EdgeList.speculative(flags))
					toPoint.arrive(1);
			}
		};
		bound.arrive(1);
		
		// We have now notified everyone that we are going to notify:
		this.waitCount = OCCURRED2;
		
		if(workItem != null) {
			if(pendingExceptions == null) {
				Intervals.POOL.submit(workItem);
				workItem = null;
			} else {
				bound.arrive(1);
			}
		}
	}
	
	/** Adds to the wait count.  Only safe when the caller is
	 *  one of the people we are waiting for.  
	 *  <b>Does not acquire a lock on this.</b> */
	protected void addWaitCount() {
		int newCount = waitCountUpdater.incrementAndGet(this);
		if(Debug.ENABLED)
			Debug.addWaitCount(this, newCount);		
	}

	/** Adds an outgoing edge, but does not adjust wait count of
	 *  target.  
	 *    
	 *  @return number of times {@link #arrive(int)} will eventually be invoked
	 *          on targetPnt (0 or 1). */
	int justAddEdgeWithoutAdjusting(PointImpl targetPnt, int flags) {
		synchronized(this) {
			primAddOutEdge(targetPnt, flags);
			if(didOccur())
				return 0; // no notification will be sent
			return 1;
		}
	}

	/** Simply adds an outgoing edge, without acquiring locks or performing any
	 *  further checks. */
	private void primAddOutEdge(PointImpl targetPnt, int flags) {
		outEdges = EdgeList.add(outEdges, targetPnt, flags);
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
				if(wc <= OCCURRED1)
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

	public void addPendingLock(GuardImpl guard, boolean exclusive) {
		LockList list = new LockList(guard, exclusive, null);
		synchronized(this) {
			list.next = pendingLocks;
			pendingLocks = list;
		}
	}

	protected void setWorkItemBeforeScheduling(IntervalImpl workItem) {
		this.workItem = workItem;
	}

	/**
	 * Adds an edge from {@code this} to {@code toImpl}, doing no safety
	 * checking, and adjusts the wait count of {@code toImpl}.
	 *  
	 * Returns the number of wait counts added to {@code toImpl}.
	 */
	public void addEdgeAndAdjust(PointImpl toImpl, int flags) {
		assert !speculative(flags) : "addEdgeAndAdjust should not be used for spec. edges!";
		
		// Note: we must increment the wait count before we release
		// the lock on this, because otherwise toImpl could arrive and
		// then decrement the wait count before we get a chance to increment
		// it.  Therefore, it's important that we do not have to acquire a lock
		// on toImpl, because otherwise deadlock could result.
		boolean occurred;
		synchronized(this) {
			primAddOutEdge(toImpl, flags);
			if(!didOccur()) {
				toImpl.addWaitCount();
				occurred = false;
			} else
				occurred = true;
		}
		
		// If we already occurred and have pending exceptions, 
		// then propagate them.
		if(!occurred && pendingExceptions != null) {
			toImpl.addPendingExceptions(pendingExceptions);
		}
	}
	
	/** Removes an edge to {@code toImpl}, returning true 
	 *  if this point has occurred. */
	public void unAddEdge(PointImpl toImpl) {
		synchronized(this) {
			EdgeList.remove(outEdges, toImpl);
		}
	}

	public void confirmEdgeAndAdjust(PointImpl toImpl, int flags) {
		boolean occurred;
		
		// Careful:
		//
		// We are going to retroactively change the flags on
		// our outEdges list.  The danger is that if a point has
		// occurred but not yet finished notifying its successors, 
		// it is still iterating over the list.  If we set the flag 
		// to non-synthetic now, then toImpl might still get notified.
		//
		// HOWEVER, that is not true if the pointer to toImpl is in the
		// first chunk of the list (which is most of the time), because
		// we save the flags from the first chunk.  Therefore, even if
		// notification is in progress, we check to see if we can find 
		// toImpl in the first chunk of the list.
		
		succeeded:
		while(true) {			
			synchronized(this) {
				int wc = waitCount;
				occurred = (wc <= OCCURRED1);
				if(!occurred) { // Did not yet occur.
					EdgeList.setFlagsInPlace(outEdges, toImpl, flags);
					toImpl.addWaitCount();
					occurred = false;
					break succeeded;
				} else if(wc == OCCURRED2) { // Fully notified.
					EdgeList.setFlagsInPlace(outEdges, toImpl, flags);
					break succeeded;
				} else if(wc == OCCURRED1) { // Not yet fully notified.
					if(EdgeList.setFlagsInPlaceOneLink(outEdges, toImpl, flags))
						break succeeded; // But we were able to update just 1st link.
				}
			}			
			
			// Just spin until the point has fully occurred.
			while(waitCount == OCCURRED1)
				Thread.yield();				
		} 
		
		if(occurred && pendingExceptions != null) {
			toImpl.addPendingExceptions(pendingExceptions);
		}		
	}
	
	PointImpl[] bounds() {
		PointImpl[] result = new PointImpl[depth];
		for(PointImpl b = bound; b != null; b = b.bound)
			result[b.depth] = b;
		return result;
	}
}
