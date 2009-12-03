package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.SAFETY_CHECKS;

class Current {
	
	private static Current root = new Current(null, null, Intervals.ROOT_END);
	private static ThreadLocal<Current> local = new ThreadLocal<Current>() {

		@Override
		protected Current initialValue() {
			return root;
		}
		
	};
	
	static Current get() {
		return local.get();
	}
	
	static Current push(PointImpl start, PointImpl end) {
		Current c = get();
		Current n = new Current(c, start, end);
		local.set(n);
		return n;
	}
	
	public Current prev;
	public PointImpl start;
	public PointImpl end;
	public IntervalImpl unscheduled;

	Current(Current prev, PointImpl start, PointImpl end) {
		this.prev = prev;
		this.start = start;
		this.end = end;
	}
	
	void addUnscheduled(IntervalImpl interval) {
		interval.nextUnscheduled = unscheduled;
		unscheduled = interval;
	}
	
	boolean isUnscheduled(PointImpl pnt) {
		return pnt.isUnscheduled(this);
	}

	void schedule(IntervalImpl interval) {
		IntervalImpl p = unscheduled;
		
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
		IntervalImpl p = unscheduled;
		while(p != null) {
			scheduleUnchecked(p);
			
			IntervalImpl n = p.nextUnscheduled;
			p.nextUnscheduled = null;
			p = n;
		}
		unscheduled = null;
	}

	private void scheduleUnchecked(IntervalImpl p) {
		assert p.start.isUnscheduled(this);
		assert p.end.isUnscheduled(this);
		
		p.task.addDependencies(p);
		p.start.clearUnscheduled();
		p.end.clearUnscheduled();
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
			if(isUnscheduled((PointImpl) to))
				return;
			if(end.hb(to))
				return;
			throw new NoEdgeException(end, to);
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
