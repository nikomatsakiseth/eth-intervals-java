package ch.ethz.intervals;

/**
 * Read-only interval interface.  This interface allows the user
 * to query the schedule but not to make changes.
 */
public interface RoInterval {
	/** Returns the parent interval */
	public RoInterval getParent();

	/** True if this is an inline interval */
	public boolean isInline();

	/** Returns the start point */
	public RoPoint getStart();
	
	/** Returns the end point */
	public RoPoint getEnd();
	
	/** True if this will hold {@code lock} when it executes */
	public boolean locks(Lock lock);
	
	/**
	 * Returns true if {@code this} is a (transitive) subinterval of {@code inter} or
	 * if {@code inter == this}. 
	 * 
	 * @param inter the parent interval.  {@code null} is considered the root
	 * interval, and therefore returns this method returns true if 
	 * {@code inter == null}. 
	 */
	public boolean isSubintervalOf(Interval inter);

	/**
	 * Returns true if {@code this} is a (transitive) inline subinterval 
	 * of {@code inter} or if {@code inter == this}.
	 * 
	 * @param inter the parent interval.  {@code null} is considered the root
	 * interval.
	 */
	public boolean isInlineSubintervalOf(Interval inter);
}
