package ch.ethz.intervals.impl;

import static ch.ethz.intervals.Intervals.SAFETY_CHECKS;
import ch.ethz.intervals.CycleException;
import ch.ethz.intervals.EdgeNeededException;
import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.MustBeBoundedByException;
import ch.ethz.intervals.NotInRootIntervalException;
import ch.ethz.intervals.IntervalException.AlreadyScheduled;

public class Current
{
	
	private static ThreadLocal<Current> local = new ThreadLocal<Current>() {

		@Override
		protected Current initialValue() {
			return new Current();
		}
		
	};
	
	public static Current get() {
		return local.get();
	}
	
	static Current push(IntervalImpl inter) {
		Current c = get();
		Current n = new Current(c, inter);
		local.set(n);
		return n;
	}
	
	public final Current prev;          /** Previous Current on stack. */
	public final IntervalImpl inter;	/** Smallest containing interval. {@code null} if root. */
	public PointImpl mr;                /** Most recent point on inter.line that occurred. Not always inter.start! May be {@code null} if root. */
	private IntervalImpl unscheduled; 	/** Linked list of unscheduled intervals. */
	
	private Current() {
		this.prev = null;
		this.inter = null;
		this.mr = null;
		this.unscheduled = null;
	}

	Current(Current prev, IntervalImpl inter) {
		assert inter != null && inter.start.didOccur();
		this.prev = prev;
		this.inter = inter;
		this.mr = inter.start;
	}
	
	void updateMostRecent(PointImpl mr) {
		assert mr.didOccur();
		this.mr = mr;
	}
	
	void addUnscheduled(IntervalImpl intervalImpl) {
		intervalImpl.nextUnscheduled = unscheduled;
		unscheduled = intervalImpl;
	}
	
	boolean isUnscheduled(PointImpl pnt) {
		IntervalImpl intervalImpl = pnt.racyInterval();
		if(intervalImpl == null)
			return false;
		return intervalImpl.isUnscheduled(this);
	}

	void schedule(IntervalImpl intervalImpl) {
		IntervalImpl p = unscheduled;
		
		if(p == intervalImpl) {
			unscheduled = intervalImpl.nextUnscheduled;
		} else {
			while(p != null && p.nextUnscheduled != intervalImpl)
				p = p.nextUnscheduled;
			
			if(p == null)
				throw new IntervalException.AlreadyScheduled(intervalImpl);
			
			p.nextUnscheduled = intervalImpl.nextUnscheduled;
		}
		
		intervalImpl.nextUnscheduled = null;
		scheduleUnchecked(intervalImpl);
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
		assert p.isUnscheduled(this);
		
		if(Debug.ENABLED)
			Debug.schedule(p, inter);
		ExecutionLog.logScheduleInterval(p);
		
		p.clearUnscheduled();
		p.start.arrive(1);
	}

	void pop() {
		assert unscheduled == null;
		local.set(prev);
	}

	void checkCanAddChild(IntervalImpl parent) {
		if(SAFETY_CHECKS) {
			if(isUnscheduled(parent.start))
				return;
			if(inter == null)
				throw new NotInRootIntervalException();
			if(inter.end.isBoundedByOrEqualTo(parent.end))
				return;
			if(inter.end.hbeq(parent.start))
				return;
			throw new EdgeNeededException(inter.end, parent.start);
		}
	}

	void checkCanAddDep(PointImpl to) {		
		if(SAFETY_CHECKS) {
			if(isUnscheduled(to))
				return;
			if(inter != null && inter.end.hbeq(to))
				return;
			throw new EdgeNeededException((inter != null ? inter.end : null), to);
		}		
	}
	
	void checkEdgeEndPointsProperlyBound(PointImpl from, PointImpl to) {
		// An edge p->q is only permitted if q is bound by
		// p's parent.  In other words, edges are permitted from
		// high-level nodes down the tree, but not the other direction.
		
		PointImpl interBound = from.interBound();
		if(interBound == null || to.isBoundedBy(interBound))
			return;
		
		throw new MustBeBoundedByException(from.bound, to);
	}

	void checkCanAddHb(PointImpl from, PointImpl to) {
		if(SAFETY_CHECKS) {
			checkCanAddDep(to);
			checkEdgeEndPointsProperlyBound(from, to);
			checkCycle(from, to);
		}
	}

	void checkCycle(PointImpl from, PointImpl to) {
		if(SAFETY_CHECKS) {
			if(from != null && to.hb(from))
				throw new CycleException(from, to);
		}
	}

	public PointImpl start() {
		if(inter != null)
			return inter.start;
		return null;
	}

}