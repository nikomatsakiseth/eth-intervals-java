package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.SAFETY_CHECKS;

class Current {
	
	private static Current root = new Current(null, Intervals.rootInter);
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
		Current n = new Current(c, inter);
		local.set(n);
		return n;
	}
	
	final Current prev;           /** Previous Current on stack. */
	final Interval inter;         /** Smallest containing interval. */
	Point start;                  /** Most recent point on inter.line that occurred. Not always inter.start! */
	final Point end;              /** Always inter.end.      */
	private Interval unscheduled; /** Linked list of unscheduled intervals. */

	Current(Current prev, Interval inter) {
		assert inter != null && inter.start.didOccur();
		this.prev = prev;
		this.inter = inter;
		this.start = inter.start;
		this.end = inter.end;
	}
	
	void updateStart(Point start) {
		assert start.didOccur();
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
