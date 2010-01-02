package ch.ethz.intervals;

import static ch.ethz.intervals.EdgeList.NONDETERMINISTIC;

final class Lock
extends /*@Writer("this.constructor")*/ Object 
implements Guard
{
	private Point latestOwner;

	/**
	 * Adds dependencies to {@code startPnt} so that it will wait
	 * to execute until the previous exclusive owner has finished.
	 * The next owner may acquire the lock once {@code startPnt.bound}
	 * has occurred.
	 */
	final void addExclusive(Point startPnt) {
		Point prevOwner;
		synchronized(this) {
			prevOwner = latestOwner;
			latestOwner = startPnt.bound;
		}
		
		if(Debug.ENABLED)
			Debug.exclusiveLock(this, prevOwner, startPnt);
		
		if(prevOwner != null)
			prevOwner.addEdgeAndAdjust(startPnt, NONDETERMINISTIC);
	}

}
