package ch.ethz.intervals.guard;

import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.RoInterval;
import ch.ethz.intervals.RoLock;
import ch.ethz.intervals.RoPoint;

/**
 * A "Guard" which permits reads or writes at any time.  
 * Fields guarded by this are unchecked 
 * with respect to race conditions and must be manually
 * inspected.     
 */
public final class RacyGuard implements StaticGuard {
	
	public static final RacyGuard Racy = new RacyGuard();
	
	private RacyGuard() {
	}
	
	@Override
	public RuntimeException checkLockable(
			RoInterval interval,
			RoLock lock) 
	{
		return null;
	}

	@Override
	public RuntimeException checkReadable(RoPoint mr, RoInterval current) 
	{
		return null;
	}
	
	@Override
	public RuntimeException ensuresFinal(RoPoint mr, RoInterval current) {
		return new IntervalException.NeverFinal(this);
	}

	@Override
	public RuntimeException checkWritable(RoPoint mr, RoInterval inter) 
	{
		return null;
	}

}
