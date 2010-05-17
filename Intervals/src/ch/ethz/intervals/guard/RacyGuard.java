package ch.ethz.intervals.guard;

import ch.ethz.intervals.mirror.Interval;
import ch.ethz.intervals.mirror.Lock;
import ch.ethz.intervals.mirror.Point;

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
			Interval interval,
			Lock lock) 
	{
		return null;
	}

	@Override
	public RuntimeException checkReadable(Point mr, Interval current) 
	{
		return null;
	}

	@Override
	public RuntimeException checkWritable(Point mr, Interval inter) 
	{
		return null;
	}

}
