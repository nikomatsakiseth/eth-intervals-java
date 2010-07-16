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
 * 
 * Although it is not prohibited,  there is never a need to 
 * instantiate this class directly, as all instances are 
 * equivalent.  You may refer to it as {@code RacyGuard#racy} in a
 * guard declaration or make use of the static field
 * {@link #racy}. 
 */
public final class RacyGuard implements Guard {
	
	public static final RacyGuard racy = new RacyGuard();

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
