package ch.ethz.intervals.guard;

import ch.ethz.intervals.RoInterval;
import ch.ethz.intervals.RoPoint;

/**
 * A guard which can not only test for accessability but which
 * can also bring it about. An example might be a Future, which 
 * joins the creating interval when accessed.
 */
public interface DynamicGuard extends Guard {
	/** 
	 * Ensures that {@code current} is permitted to write data protected
	 * by this guard.  Throws an exception if this attempt fails.
	 * 
	 * @throws IntervalException if current cannot be made writable */
	public void makeWritableBy(RoPoint mr, RoInterval current);
	
	/**
	 * @see #makeWritableBy(RoPoint, RoInterval)
	 */
	public void makeReadableBy(RoPoint mr, RoInterval current);

	/**
	 * @see #makeFinal(RoPoint, RoInterval)
	 */
	public void makeFinal(RoPoint mr, RoInterval current);
}
