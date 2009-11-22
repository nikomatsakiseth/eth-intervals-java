package ch.ethz.intervals;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Set;

import ch.ethz.intervals.ThreadPool.Worker;

abstract class PointImpl implements Point {
	
	public static final int OCCURRED = -1;
	
	final PointImpl bound;
	final int depth;
	protected EdgeList outEdges;
	protected int waitCount;

	protected Throwable throwable;

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

	protected void arrive(int cnt) {
		int newCount;
		synchronized(this) {
			newCount = this.waitCount - cnt;
			this.waitCount = newCount;
		}
		
		if(Debug.ENABLED) {
			Debug.arrive(this, cnt, newCount);
		}		
		
		if(newCount == 0)
			occur();
	}
	
	protected abstract void occur();
	
	/**
	 * The default impl. of occur():
	 * <ul>
	 * <li> Sets the wait count to {@link #OCCURRED},
	 * <li> Passes along any throwable to the bound
	 * <li> Notifies any outgoing edges that we have arrived, 
	 *      but <b>does not notify the bound!</b>
	 * </ul>
	 * These are the tasks shared by {@link StartPointImpl} and
	 * {@link AsyncPointImpl}.
	 */
	protected void defaultOccur() {
		final EdgeList outEdges;
		synchronized(this) {
			outEdges = this.outEdges;
			this.waitCount = OCCURRED;
		}
		
		ExecutionLog.logArrive(this);

		if(throwable != null)
			bound.setThrowableFromChild(throwable);
		
		for(EdgeList outEdge = outEdges; outEdge != null; outEdge = outEdge.next)
			outEdge.toPoint.arrive(1);		
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

}
