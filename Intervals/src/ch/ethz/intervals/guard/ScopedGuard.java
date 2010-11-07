package ch.ethz.intervals.guard;

import ch.ethz.intervals.Condition;
import ch.ethz.intervals.Interval;

public class ScopedGuard implements Guard {
	private final Interval inter;
	private final Guard guard;

	public ScopedGuard(Interval inter, Guard guard) {
		this.inter = inter;
		this.guard = guard;
	}

	@Override
	public Condition condReadableBy() {
		return inter.condSubintervalOfOrEqualTo().and(guard.condReadableBy());
	}

	@Override
	public Condition condWritableBy() {
		return inter.condSubintervalOfOrEqualTo().and(guard.condWritableBy());
	}

	@Override
	public Condition condFinal() {
		return inter.getEnd().condDidOccurWithoutError();
	}

}