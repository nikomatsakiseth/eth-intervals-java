package ch.ethz.intervals;

import ch.ethz.intervals.guard.Guard;

/** Read-only point interface. */
public interface RoPoint {

	/** 
	 * Returns the bound of this point or null
	 * if the bound is the end of the root interval.
	 * 
	 * Note: At compile time, this function
	 * may return an approximation of the actual
	 * bound (in other words, it may not return
	 * the closest bound). */
	public RoPoint getBound();
	
	/** 
	 * Returns an array {@code bounds} where 
	 * {@code bounds[0]} == end of the root interval
	 * code the last element is {@code this}. 
	 * 
	 * Note: At compile time, this function
	 * may return an approximation of the actual
	 * bound array (in other words, some bounds
	 * may not be present). */
	public RoPoint[] getBounds();
	
	/**
	 * Returns the mutual bound of {@code this} and
	 * {@code pnt}. 
	 * 
	 * Note: At compile time, this function may
	 * not return the precise mutual bound but
	 * rather a bound of the mutual bound. */
	public RoPoint mutualBound(RoPoint pnt);

	/**
	 * True if {@code pnt} bounds this point.
	 * Equivalent to searching the result of
	 * {@link #getBounds()} for
	 * {@code pnt}, except that it is false 
	 * if {@code pnt == this}. */
	public boolean isBoundedBy(RoPoint pnt);
	
	public boolean hb(RoPoint pnt);
	
	public boolean hbeq(RoPoint pnt);

	/**
	 * True if this point acquires the lock {@code lock} to protect
	 * {@code guard} before occurring. 
	 * 
	 * @param lock the lock being acquired
	 * @param guard if non-null, the guard being protected.
	 * 
	 * @see RoInterval#locks(RoLock, Guard) */
	public boolean acquiresLock(RoLock lock, Guard guard);

	/**
	 * True if this point releases the lock {@code lock} to protect 
	 * {@code guard} after occurring. 
	 * 
	 * @param lock the lock being acquired
	 * @param guard if non-null, the guard being protected.
	 * 
	 * @see RoInterval#locks(RoLock, Guard) */
	public boolean releasesLock(RoLock lock, Guard guard);

}
