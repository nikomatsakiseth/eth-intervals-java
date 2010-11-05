package ch.ethz.intervals.tsp;

import ch.ethz.intervals.Condition;
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
	public Condition condReadableBy() {
		return search.condSubintervalOfOrEqualTo();
	}

	@Override
	public Condition condWritableBy() {
		return search.condSubintervalOfOrEqualTo().and(lock.condHeld());
	}

	@Override
	public Condition condFinal() {
		return search.getEnd().condDidOccurWithoutError();
	}

}
