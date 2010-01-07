package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.SAFETY_CHECKS;

class Current {
	
	private static Current root = new Current(null, null, Intervals.rootEnd);
	private static ThreadLocal<Current> local = new ThreadLocal<Current>() {

		@Override
		protected Current initialValue() {
			return root;
		}
		
	};
	
	static Current get() {
		return local.get();
	}
	
	static Current push(Interval inter) {
		Current c = get();
		Current n = new Current(c, inter.start, inter.end);
		local.set(n);
		return n;
	}
	
	final Current prev;           /** */
	final Line line;              /** The line we are executing. */
	Point start;                  /** An optional point which HB the current moment. */
	final Point end;              /** Bound of the current moment. */
	private Interval unscheduled; /** Linked list of unscheduled intervals. */

	Current(Current prev, Point start, Point end) {
		assert end != null;
		this.prev = prev;
		this.line = end.line;
		this.start = start;
		this.end = end;
	}
	
	void updateStart(Point start) {
		assert start.nextEpochOrBound() == end;
		this.start = start;
	}
	
	void addUnscheduled(Interval interval) {
		interval.nextUnscheduled = unscheduled;
		unscheduled = interval;
	}
	
	boolean isUnscheduled(Point pnt) {
		return pnt.line.isUnscheduled(this);
	}

	void schedule(Interval interval) {
		Interval p = unscheduled;
		
		if(p == interval) {
			unscheduled = interval.nextUnscheduled;
		} else {
			while(p != null && p.nextUnscheduled != interval)
				p = p.nextUnscheduled;
			
			if(p == null)
				throw new AlreadyScheduledException();
			
			p.nextUnscheduled = interval.nextUnscheduled;
		}
		
		interval.nextUnscheduled = null;
		scheduleUnchecked(interval);
	}

	void schedule() {
		Interval p = unscheduled;
		while(p != null) {
			scheduleUnchecked(p);
			
			Interval n = p.nextUnscheduled;
			p.nextUnscheduled = null;
			p = n;
		}
		unscheduled = null;
	}

	private void scheduleUnchecked(Interval p) {
		assert p.line.isUnscheduled(this);
		
		ExecutionLog.logScheduleInterval(p);
		
		p.line.clearUnscheduled();
		p.start.arrive(1);
	}

	void pop() {
		assert unscheduled == null;
		local.set(prev);
	}

	void checkCanAddDep(Point to) {		
		if(SAFETY_CHECKS) {
			if(end == to)
				return;
			if(isUnscheduled((Point) to))
				return;
			if(end.hb(to))
				return;
			throw new EdgeNeededException(end, to);
		}		
	}

	void checkCanAddHb(Point from, Point to) {
		if(SAFETY_CHECKS) {
			checkCanAddDep(to);
			checkCycle(from, to);
		}
	}

	void checkCycle(Point from, Point to) {
		if(SAFETY_CHECKS) {
			if(from != null && to.hb(from))
				throw new CycleException(from, to);
		}
	}

}
