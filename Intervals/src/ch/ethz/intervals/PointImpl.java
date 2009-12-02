package ch.ethz.intervals;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.atomic.AtomicIntegerFieldUpdater;

import ch.ethz.intervals.ThreadPool.Worker;

class PointImpl implements Point {
	
	public static final int OCCURRED = -1;	
	public static final int initialWaitCount = Integer.MAX_VALUE;
	
	private static AtomicIntegerFieldUpdater<PointImpl> waitCountUpdater =
		AtomicIntegerFieldUpdater.newUpdater(PointImpl.class, "waitCount");

	/** If this flag is set, then uncaught exceptions for this 
	 *  interval are NOT propagated to its parent. */
	protected static final int FLAG_MASK_EXC = 1;
	
	final PointImpl bound;                  /** Bound of this point (this->bound). */
	final int depth;                        /** Depth in point bound tree. */
	private EdgeList outEdges;              /** Linked list of outgoing edges from this point. */
	private volatile int waitCount;         /** Number of preceding points that have not arrived.  
	                                            Set to {@link #OCCURRED} when this has occurred. 
	                                            Modified only through {@link #waitCountUpdater}. */
	private Throwable pendingException;     /** Exception that occurred while executing the task or in some preceding point. */
	private int flags;                      /** Flags (see integer constants like {@link #FLAG_MASK_EXC}) */
	private LockList pendingLocks;          /** Locks to acquire once waitCount reaches 0 but before we occur */
	private ThreadPool.WorkItem workItem;   /** Work to schedule when we occur (if any) */
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
		
		if(bound != null) {
			while(pp.depth > bound.depth)
				pp = pp.bound;
			
			return (pp == bound);
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
		return hb(p, true);
	}
	
	/** true if {@code this} -> {@code p}.
	 * @param p another point
	 * @param onlyDeterministic false to exclude edges
	 * that may depend on scheduler, primarily this means
	 * those resulting from locks */
	boolean hb(final Point p, boolean onlyDeterministic) {
		assert p != null;
		
		if(p == this)
			return false;
		if(p == bound)
			return true;

		// Perform a BFS to find p:
		
		final Queue<PointImpl> queue = new LinkedList<PointImpl>();
		final Set<PointImpl> visited = new HashSet<PointImpl>();
		queue.add(this);
		
		class Helper {
			/** Add {@code q} to queue if not already visited, returning
			 *  {@code true} if {@code q} is the node we are looking for. */
			boolean tryEnqueue(PointImpl q) {
				assert q != null;
				
				if(q == p)
					return true; // found it
				
				if(visited.add(q))
					queue.add(q);
				return false;
			}
		}
		Helper h = new Helper();
		
		while(!queue.isEmpty()) {
			PointImpl q = queue.remove();
			
			if(q.bound != null && h.tryEnqueue(q.bound))				
				return true;
			
			for(PointImpl toPoint : EdgeList.edges(q.outEdgesSync(), onlyDeterministic)) {
				if(h.tryEnqueue(toPoint))
					return true;
			}
		}
		
		return false;
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
		
		final EdgeList outEdges;
		final int chunkMask;
		synchronized(this) {
			outEdges = this.outEdges;
			chunkMask = EdgeList.chunkMask(outEdges);
			this.waitCount = OCCURRED;
			notifyAll(); // in case anyone is joining us
		}
		
		ExecutionLog.logArrive(this);
		
		if((flags & FLAG_MASK_EXC) == 0 && pendingException != null)
			bound.setPendingExceptionFromChild(pendingException);
		
		Iterable<PointImpl> notifiedPoints = EdgeList.edges(outEdges, chunkMask, false);
		if(Debug.ENABLED)
			Debug.occur(this, notifiedPoints);
		
		for(PointImpl toPoint : notifiedPoints)
			toPoint.arrive(1);
		bound.arrive(1);
		
		if(workItem != null) {
			Intervals.POOL.submit(workItem);
			workItem = null;
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
	int addEdgeWithoutAdjustingWaitCount(PointImpl targetPnt, boolean deterministic) {
		synchronized(this) {
			primAddOutEdge(targetPnt, deterministic);
			if(waitCount == OCCURRED)
				return 0; // no notification will be sent
			return 1;
		}
	}

	/** Simply adds an outgoing edge, without acquiring locks or performing any
	 *  further checks. */
	private void primAddOutEdge(PointImpl targetPnt, boolean deterministic) {
		outEdges = EdgeList.add(outEdges, targetPnt, deterministic);
	}

	/** Adds to the wait count but only if the point has
	 *  not yet occurred.  Always safe.
	 *  @return true if the add succeeded. */
	synchronized boolean tryAddWaitCount() {
		if(waitCount == OCCURRED)
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
				while(waitCount != OCCURRED)
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

	/** Invoked when the task for which this point is an end point
	 *  results in an error. */
	protected synchronized void setPendingException(Throwable thr) {
		this.pendingException = thr; // Note: may overwrite a Throwable from a child.
	}
	
	/** Checks if a pending throwable is stored and throws a
	 *  {@link RethrownException} if so. */
	void checkAndRethrowPendingException() {
		if(pendingException != null)
			throw new RethrownException(pendingException);				
	}

	/** Invoked when a child interval has thrown the exception 't'. 
	 *  Overwrites the field {@link #pendingException} with {@code t}.
	 *  This is non-ideal, as it may result in exceptions being lost,
	 *  and it is also unpredictable which exception will be reported
	 *  (the one from the task? the one from a child? if so, which child)
	 *  The correct behavior is probably to construct an aggregate object
	 *  containing a set of all exceptions thrown.
	 */
	synchronized void setPendingExceptionFromChild(Throwable thr) {
		if(bound != null)
			setPendingException(thr);
		else
			thr.printStackTrace();
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

	protected void setWorkItemBeforeScheduling(ThreadPool.WorkItem workItem) {
		this.workItem = workItem;
	}

	/**
	 * Adds an edge from {@code this} to {@code toImpl}, doing no safety
	 * checking, and adjusts the wait count of {@code toImpl}.
	 *  
	 * Returns the number of wait counts added to {@code toImpl}.
	 */
	public int addEdgeAndAdjustWaitCount(PointImpl toImpl, boolean deterministic) {
		// Note: we must increment the wait count before we release
		// the lock on this, because otherwise toImpl could arrive and
		// then decrement the wait count before we get a chance to increment
		// it.  Therefore, it's important that we do not have to acquire a lock
		// on toImpl, because otherwise deadlock could result.
		synchronized(this) {
			primAddOutEdge(toImpl, deterministic);
			if(waitCount != OCCURRED) {
				toImpl.addWaitCount();
				return 1;
			}
			return 0;
		}
	}

	public void unAddEdge(PointImpl toImpl) {
		synchronized(this) {
			EdgeList.remove(outEdges, toImpl);
		}
	}

	public void confirmEdge(PointImpl toImpl) {
		synchronized(this) {
			EdgeList.setDeterministic(outEdges, toImpl);
		}
	}
}
