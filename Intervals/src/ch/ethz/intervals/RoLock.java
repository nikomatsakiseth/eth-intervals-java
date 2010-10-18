package ch.ethz.intervals;

import ch.ethz.intervals.guard.StaticGuard;

public interface RoLock extends StaticGuard {
	
	/** A condition that is true for intervals that (will) hold this lock */
	public Condition condHeld(); 

}
