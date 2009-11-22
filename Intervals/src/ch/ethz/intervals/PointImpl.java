package ch.ethz.intervals;

import static ch.ethz.intervals.UnscheduledIntervalImpl.initialWaitCount;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Set;

import ch.ethz.intervals.ThreadPool.Worker;

abstract class PointImpl implements Point {
	
	public static final int OCCURRED = -1;

	/** If this flag is set, then uncaught exceptions for this 
	 *  interval are NOT propagated to its parent. */
	protected static final int FLAG_MASK_EXC = 1;
	
	final PointImpl bound;
	final int depth;
	protected EdgeList outEdges;
	protected int waitCount;

	protected Throwable throwable;

	protected int flags;

	/** Points at the next lock which we must acquire before we can START, or null. */
	protected LockList pendingLocks;

	protected ThreadPool.WorkItem workItem;

	/** Used only for the end point of the root interval. */
	PointImpl(int waitCount) {
		bound = null;
		depth = 0;
		this.waitCount = waitCount;
	}

	/** Used only for the end point of the root interval. */
	PointImpl(PointImpl bound, int waitCount) {
		this.bound = bound;
		this.depth = bound.depth + 1;
		this.waitCount = waitCount;
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
			
			for(EdgeList edgeList = q.outEdges; edgeList != null; edgeList = edgeList.next) {
				if(onlyDeterministic && !edgeList.determinstic)
					continue;
				if(h.tryEnqueue(edgeList.toPoint))
					return true;
			}
		}
		
		return false;
	}

	protected final void arrive(int cnt) {
		int newCount;
		synchronized(this) {
			newCount = this.waitCount - cnt;
			this.waitCount = newCount;
		}
		
		if(Debug.ENABLED) {
			Debug.arrive(this, cnt, newCount);
		}		
		
		if(newCount == 0 && cnt != 0)
			occur();
	}
	
	protected final void occur() {
		// Now that all interval dependencies are resolved, insert
		// ourselves into the queue for the locks we desire (if any).
		if(pendingLocks != null) {
			// Temporarily make this large so it doesn't drop to zero while we work.
			// Note that everyone who might adjust this count has already arrived.
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
			return;
		} 
		
		// No pending locks, this is really happening:		
		final EdgeList outEdges;
		synchronized(this) {
			outEdges = this.outEdges;
			this.waitCount = OCCURRED;
			notifyAll(); // in case anyone is joining us
		}
		
		ExecutionLog.logArrive(this);
		
		if((flags & FLAG_MASK_EXC) == 0 && throwable != null)
			bound.setThrowableFromChild(throwable);
		
		for(EdgeList outEdge = outEdges; outEdge != null; outEdge = outEdge.next)
			outEdge.toPoint.arrive(1);
		
		bound.arrive(1);
		
		if(workItem != null) {
			Intervals.POOL.submit(workItem);
			workItem = null;
		}
	}
	
	protected void addWaitCount() {
		int newCount;
		synchronized(this) {
			assert waitCount >= 1;
			newCount = ++waitCount;
		}
		if(Debug.ENABLED)
			Debug.addWaitCount(this, newCount);		
	}

	synchronized int addOutEdge(PointImpl targetPnt, boolean deterministic) {
		addOutEdgeDuringConstruction(targetPnt, deterministic);
		if(waitCount == OCCURRED)
			return 0; // no notification will be sent
		return 1;
	}

	void addOutEdgeDuringConstruction(PointImpl targetPnt, boolean deterministic) {
		outEdges = new EdgeList(targetPnt, deterministic, outEdges);
	}

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
			while(waitCount != OCCURRED) {
				if(!worker.doWork(false))
					Thread.yield();				
			}
		}
	}

	protected synchronized void setThrowable(Throwable thr) {
		this.throwable = thr; // Note: may overwrite a Throwable from a child.
	}
	
	void checkThrowable() {
		if(throwable != null)
			throw new RethrownException(throwable);				
	}

	/** Invoked when a child interval has thrown the exception 't'. 
	 *  Overwrites the field {@link #throwable} with {@code t}.
	 *  This is non-ideal, as it may result in exceptions being lost,
	 *  and it is also unpredictable which exception will be reported
	 *  (the one from the task? the one from a child? if so, which child)
	 *  The correct behavior is probably to construct an aggregate object
	 *  containing a set of all exceptions thrown.
	 */
	synchronized void setThrowableFromChild(Throwable thr) {
		if(bound != null)
			setThrowable(thr);
		else
			thr.printStackTrace();
	}

	protected void addFlagBeforeScheduling(int flag) {
		this.flags = this.flags | flag;
	}

	protected void removeFlagBeforeScheduling(int flag) {
		this.flags = this.flags & (~flag);
	}

	protected void setPendingLocksBeforeScheduling(LockList pendingLocks) {
		this.pendingLocks = pendingLocks;
	}

	protected void setWorkItemBeforeScheduling(ThreadPool.WorkItem workItem) {
		this.workItem = workItem;
	}

}
