package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.checkCurrentIntervalEndHbOrSame;
import static ch.ethz.intervals.Intervals.checkEdgeOrSame;

class UnscheduledIntervalImpl implements UnscheduledInterval {
	
	final static int initialWaitCount = Integer.MAX_VALUE;
	final static int START_SHIFT = 0, END_SHIFT = 32;
	
	private LockList firstLock = null, lastLock = null;
	private PointImpl start;
	
	private PointImpl end;
		
	private long desiredWaitCounts;
	
	private StringBuilder description;
	
	public UnscheduledIntervalImpl(PointImpl endBound) {
		IntervalImpl<?> current = Intervals.currentInterval.get();
		checkEdgeOrSame(current.end, endBound);
		
		this.end = new PointImpl(endBound, initialWaitCount);
		this.start = new PointImpl(this.end, initialWaitCount);
		this.desiredWaitCounts = (2L << END_SHIFT); // end always waits for start and for task to finish.
		
		endBound.addWaitCount();
		current.start.addOutEdge(start, true);
		
		ExecutionLog.logNewInterval(current.start, start, end);
		
		if(Debug.ENABLED)
			description = new StringBuilder();
	}
	

	@Override
	public UnscheduledInterval startAfter(Point p) {
		// Important: add no code to this method so as to ensure proper error recovery.
		return addAfterDep(start, p, START_SHIFT);
	}
	
	@Override
	public UnscheduledInterval endAfter(Point p) {
		// Important: add no code to this method so as to ensure proper error recovery.
		return addAfterDep(end, p, END_SHIFT);
	}
	
	private UnscheduledInterval addAfterDep(PointImpl side, Point p, int shift) {
		checkUnscheduled();
		if(p != null) {
			boolean completedWithError = true;
			try {
				PointImpl pp = (PointImpl) p;
				checkCycle(pp, side);

				long wait = pp.addOutEdge(side, true); // n.b.: use long so that << doesn't lose bits
				desiredWaitCounts += (wait << shift);
				
				ExecutionLog.logEdge(p, side);
				if(Debug.ENABLED)
					description.append(String.format("[%s ->(%s) %s]", p, wait, side));

				completedWithError = false;
			} finally {
				if(completedWithError)
					recoverFromError();
			} 
		}
		return this;
	}

	@Override
	public UnscheduledInterval startBefore(Point p) {
		// Important: add no code to this method so as to ensure proper error recovery.
		return addBeforeDep(start, p);
	}

	@Override
	public UnscheduledInterval endBefore(Point p) {
		// Important: add no code to this method so as to ensure proper error recovery.
		return addBeforeDep(end, p);
	}

	private UnscheduledInterval addBeforeDep(PointImpl side, Point p) {
		checkUnscheduled();
		if(p != null) {
			boolean completedWithError = true;
			try {
				checkCurrentIntervalEndHbOrSame(p);
				PointImpl pp = (PointImpl) p;
				checkCycle(side, pp);
				
				// Subtle: Call addOutEdgeDuringConstruction() first so that, if an
				// out of memory error occurs, we have not yet incremented the 
				// wait count.  Incrementing the wait count can't fail.
				side.addOutEdgeDuringConstruction(pp, true);
				pp.addWaitCount();
				
				ExecutionLog.logEdge(side, p);
				if(Debug.ENABLED)
					description.append(String.format("[%s -> %s]", side, p));
			
				completedWithError = false;
			} finally {
				if(completedWithError)
					recoverFromError();
			} 
		}
		return this;
	}

	UnscheduledInterval addLock(GuardImpl guard, boolean exclusive) {
		boolean completedWithError = true;
		checkUnscheduled();
		
		try {			
			// We want to add guard (potentially exclusive), and all of its ancestor locks (shared), 
			// to the end of the list of locks.  In other words, if the list of locks is:
			// 
			//     l1...lN
			//
			// before, then it should be:
			//
			//     l1...lN, a1...aN, guard
			// 
			// after, where a1...aN are the ancestors of 'guard'.  The key here is that order is
			// significant for deadlocks, so we preserve the order the user gave us.
			
			LockList m = new LockList(guard, exclusive, null);
			
			LockList n = m;
			/* For now, remove nesting of guards.  It's annoying anyhow.
			for(guard = guard.parentGuard; guard != null; guard = guard.parentGuard)
				n = new LockList(guard, false, n);
			*/
			
			if(lastLock != null)
				lastLock.next = n;
			else
				firstLock = n;			
			lastLock = m;
			
			ExecutionLog.logLock(start, guard);
			if(Debug.ENABLED)
				description.append(String.format("[%sLock(%s)]", (exclusive ? "ex" : "sh"), guard));
			
			completedWithError = false;
			return this;
		} finally {
			if(completedWithError)
				recoverFromError();
		}
	}
	
	@Override
	public UnscheduledInterval exclusiveLock(Guard g) {
		// Important: add no code to this method so as to ensure proper error recovery.
		return addLock((GuardImpl) g, true);
	}

	private void recoverFromError() {
		// We always order our operations so that, should an error occur,
		// we are still in a runnable state (or at least I think we do).
		// This means that if we ever encounter an error during construction, 
		// we can go ahead and schedule "as is" but with an empty task.
		// This way the interval execution as a whole proceeds even if this interval
		// is doomed.
		schedule(Intervals.emptyTask);
	}

	@Override
	public <V> Interval<V> schedule(Task<V> task) {
		checkUnscheduled();
		
		PointImpl start = this.start;
		this.start = null;
		
		PointImpl end = this.end;
		this.end = null;
		
		IntervalImpl<V> inter = new IntervalImpl<V>(task, start, end);
		start.setPendingLocksBeforeScheduling(firstLock);
		start.setWorkItemBeforeScheduling(inter);
		
		ExecutionLog.logScheduleInterval(start, task);
		if(Debug.ENABLED)
			Debug.newInterval(inter, description.toString());
		
		start.arrive(initialWaitCount - (int)(desiredWaitCounts >> START_SHIFT));
		end.arrive(initialWaitCount - (int)(desiredWaitCounts >> END_SHIFT));
		
		return inter;
	}

	private void checkUnscheduled() {
		if(start == null)
			throw new AlreadyScheduledException();
	}

	// check that creating an edge from->to does not create a cycle
	private void checkCycle(PointImpl from, PointImpl to) {
		if(Intervals.SAFETY_CHECKS) {
			// Note: Right now, we only catch deterministic cycles. 
			// Cycles created via locks can still cause deadlock.
			if(to.hb(from))
				throw new CycleException(from, to);
		}
	}

	@Override
	public UnscheduledIntervalImpl setMaskExceptions(boolean maskExceptions) {
		checkUnscheduled();
		
		if(maskExceptions)
			end.addFlagBeforeScheduling(PointImpl.FLAG_MASK_EXC);
		else
			end.removeFlagBeforeScheduling(PointImpl.FLAG_MASK_EXC);
		return this;
	}

}
