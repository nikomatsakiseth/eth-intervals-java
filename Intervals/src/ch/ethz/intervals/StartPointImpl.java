package ch.ethz.intervals;

import static ch.ethz.intervals.UnscheduledIntervalImpl.initialWaitCount;

class StartPointImpl extends PointImpl {

	/** Points at the next lock which we must acquire before we can START, or null. */
	private LockList pendingLocks;
	private ThreadPool.WorkItem workItem;
	
	public StartPointImpl(PointImpl bound, int waitCount) {
		super(bound, waitCount);
	}
	
	void setPendingLocksBeforeScheduling(LockList pendingLocks) {
		this.pendingLocks = pendingLocks;
	}
	
	void setWorkItemBeforeScheduling(ThreadPool.WorkItem workItem) {
		this.workItem = workItem;
	}
	
	protected void occur() {
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
		
		defaultOccur();
		
		if(workItem != null) {
			Intervals.POOL.submit(workItem);
			workItem = null;
		}
	}
	
	@Override
	public String toString() {
		return String.format("Start(%x)", System.identityHashCode(this));
	}
	
}
