package ch.ethz.intervals;

import ch.ethz.intervals.params.Parent;

@Parent
class GuardImpl // Would be final, but for Mockito 
extends /*@Identity("RO")*/ Object implements Guard
{
	protected PointImpl latestOwner;
	protected boolean latestOwnerIsSharedInterval;

	/**
	 * Adds dependencies to {@code inter} so that it will wait
	 * to execute until the previous exclusive owner has finished.
	 * @return 1 if inter needs to wait, 0 otherwise  
	 */
	protected int addExclusive(PointImpl startPnt) {
		PointImpl prevOwner;
		synchronized(this) {
			prevOwner = latestOwner;
			latestOwner = startPnt.bound;
			latestOwnerIsSharedInterval = false;
		}
		
		if(Debug.ENABLED)
			Debug.exclusiveLock(this, prevOwner, startPnt);
		
		if(prevOwner == null)
			return 0;
		else {
			return prevOwner.addOutEdge(startPnt, false);
		}
	}
	
	protected void addShared(PointImpl startPnt) {
		PointImpl prevOwner;
		int wait;
		synchronized(this) {			
			prevOwner = latestOwner;
			if(latestOwnerIsSharedInterval) {
				if(prevOwner.tryAddWaitCount()) {
					startPnt.bound.addOutEdge(prevOwner, false);
					if(Debug.ENABLED)
						Debug.sharedLock(this, null, prevOwner, startPnt);
					return; // can safely start immediately
				}
			}
			
			final int initialWaitCount = 2;
			latestOwner = new AsyncPointImpl(null, Intervals.ROOT_END, initialWaitCount);
			latestOwnerIsSharedInterval = true;
			wait = prevOwner.addOutEdge(latestOwner, false);
			wait += startPnt.bound.addOutEdge(latestOwner, false);
		}
		latestOwner.arrive(2 - wait);
		if(Debug.ENABLED)
			Debug.sharedLock(this, prevOwner, latestOwner, startPnt);
		return;
	}

}
