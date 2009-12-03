package ch.ethz.intervals;

import static ch.ethz.intervals.EdgeList.NONDETERMINISTIC;
import ch.ethz.intervals.params.Parent;

@Parent
class GuardImpl // Would be final, but for Mockito 
extends /*@Identity("RO")*/ Object implements Guard
{
	private PointImpl latestOwner;

	/**
	 * Adds dependencies to {@code inter} so that it will wait
	 * to execute until the previous exclusive owner has finished.
	 * @return 1 if inter needs to wait, 0 otherwise  
	 */
	protected void addExclusive(PointImpl startPnt) {
		PointImpl prevOwner;
		synchronized(this) {
			prevOwner = latestOwner;
			latestOwner = startPnt.bound;
		}
		
		if(Debug.ENABLED)
			Debug.exclusiveLock(this, prevOwner, startPnt);
		
		if(prevOwner != null) {
			prevOwner.addEdgeAndAdjust(startPnt, NONDETERMINISTIC);
		}
	}

}
