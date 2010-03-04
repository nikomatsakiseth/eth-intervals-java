package ch.ethz.intervals.guard;

import ch.ethz.intervals.mirror.IntervalMirror;
import ch.ethz.intervals.mirror.LockMirror;
import ch.ethz.intervals.mirror.PointMirror;

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
			IntervalMirror interval,
			LockMirror lock) 
	{
		return null;
	}

	@Override
	public RuntimeException checkReadable(PointMirror mr, IntervalMirror current) 
	{
		return null;
	}

	@Override
	public RuntimeException checkWritable(PointMirror mr, IntervalMirror inter) 
	{
		return null;
	}

}
