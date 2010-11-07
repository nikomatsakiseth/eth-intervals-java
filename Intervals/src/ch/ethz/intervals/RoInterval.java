package ch.ethz.intervals;

import ch.ethz.intervals.guard.Guard;
import ch.ethz.intervals.guard.StaticGuard;

/**
 * Read-only interval interface.  This interface allows the user
 * to query the schedule but not to make changes.
 */
public interface RoInterval extends StaticGuard {
	/** Returns the parent interval */
	public RoInterval getParent();

	/** True if this is an inline interval */
	public boolean isInline();

	/** Returns the start point */
	public RoPoint getStart();
	
	/** Returns the end point */
	public RoPoint getEnd();
	
	/** 
	 * True if this will lock {@code guard} with {@code lock} when it executes.
	 *  
	 * @param guard The guard with which the lock was associated when it
	 * was added to the interval.  If {@code null}, then the method returns
	 * true if the lock is held at all, regardless of the guard it was associated
	 * with. */
	public boolean locks(RoLock lock, Guard guard);
	
	/**
	 * Returns true if {@code this} is a (transitive) subinterval of {@code inter} or
	 * if {@code inter == this}. 
	 * 
	 * @param inter the parent interval.  {@code null} is considered the root
	 * interval, and therefore returns this method returns true if 
	 * {@code inter == null}. 
	 */
	public boolean isSubintervalOfOrEqualTo(RoInterval inter);

	/**
	 * Returns true if {@code this} is a (transitive) inline subinterval 
	 * of {@code inter} or if {@code inter == this}.
	 * 
	 * @param inter the parent interval.  {@code null} is considered the root
	 * interval.
	 */
	public boolean isInlineSubintervalOfOrEqualTo(RoInterval inter);
	
	public Condition condSubintervalOfOrEqualTo();
	public Condition condIsInlineSubintervalOfOrEqualTo();
	
}
