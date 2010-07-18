package ch.ethz.intervals.guard;

import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.RoInterval;
import ch.ethz.intervals.RoLock;
import ch.ethz.intervals.RoPoint;

public final class FinalGuard implements StaticGuard {
	
	public static final FinalGuard Final = new FinalGuard();
	
	private FinalGuard() {
	}

	@Override
	public RuntimeException checkWritable(RoPoint mr, RoInterval inter) {
		return new IntervalException.NeverPermitsWrites(this);
	}

	@Override
	public RuntimeException checkReadable(RoPoint mr, RoInterval current) {
		return null;
	}

	@Override
	public RuntimeException ensuresFinal(RoPoint mr, RoInterval current) {
		return null;
	}

	@Override
	public RuntimeException checkLockable(RoInterval interval, RoLock lock) {
		return new IntervalException.CannotBeLockedBy(this, lock);
	}

}
