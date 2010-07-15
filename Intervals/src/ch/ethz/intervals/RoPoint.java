package ch.ethz.intervals;

/** Read-only point interface. */
public interface RoPoint {

	/** 
	 * Returns the bound of this point. */
	public RoPoint bound();
	
	/** 
	 * Returns an array {@code bounds} where 
	 * {@code bounds[0]} == end of the root interval
	 * code the last element is {@code this}. */
	public RoPoint[] getBounds();
	
	/**
	 * Returns the mutual bound of {@code this} and
	 * {@code pnt}. */
	public RoPoint mutualBound(RoPoint pnt);
	
	public boolean isBoundedBy(RoPoint pnt);
	
	public boolean hb(RoPoint pnt);
	
	public boolean hbeq(RoPoint pnt);
	

}
