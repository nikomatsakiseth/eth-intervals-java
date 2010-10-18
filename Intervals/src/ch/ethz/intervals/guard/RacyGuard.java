package ch.ethz.intervals.guard;

import ch.ethz.intervals.Condition;


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
	public Condition condReadableBy() {
		return Condition.TRUE;
	}

	@Override
	public Condition condWritableBy() {
		return Condition.TRUE;
	}

	@Override
	public Condition condFinal() {
		return Condition.FALSE;
	}
	
}
