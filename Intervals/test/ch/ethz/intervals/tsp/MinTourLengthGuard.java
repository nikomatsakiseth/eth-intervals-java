package ch.ethz.intervals.tsp;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.Lock;
import ch.ethz.intervals.RoInterval;
import ch.ethz.intervals.RoLock;
import ch.ethz.intervals.RoPoint;
import ch.ethz.intervals.guard.Guard;

public class MinTourLengthGuard implements Guard {
	
	private final Interval search;
	private final Lock lock;

	public MinTourLengthGuard(Interval search, Lock lock) {
		this.search = search;
		this.lock = lock;
	}

	@Override
	public RuntimeException checkWritable(RoPoint mr, RoInterval current) {
		if(!current.isSubintervalOfOrEqualTo(search))
			return new IntervalException.NotSubinterval(current, search);
		if(!current.locks(lock, null))
			return new IntervalException.LockNotHeld(lock, this, current);
		return null;
	}

	@Override
	public RuntimeException checkReadable(RoPoint mr, RoInterval current) {
		// Note: readable without lock!
		if(!current.isSubintervalOfOrEqualTo(search))
			return ensuresFinal(mr, current);
		return null;
	}

	@Override
	public RuntimeException ensuresFinal(RoPoint mr, RoInterval current) {
		if(!search.getEnd().hbeq(mr))
			return new IntervalException.MustHappenBefore(search.getEnd(), mr);
		return null;
	}

	@Override
	public RuntimeException checkLockable(
			RoPoint acq, 
			RoInterval interval,
			RoLock lock) 
	{
		return null;
	}

	

}
