package ch.ethz.intervals;

import static ch.ethz.intervals.EdgeList.NONDETERMINISTIC;

abstract class LockBase 
{
	private Point latestOwner;
	
	abstract protected Lock lock();

	/**
	 * Adds dependencies to {@code startPnt} so that it will wait
	 * to execute until the previous exclusive owner has finished.
	 * The next owner may acquire the lock once {@code startPnt.bound}
	 * has occurred.
	 */
	final void addExclusive(Point startPnt, Point endPnt) {
		Point prevOwner;
		synchronized(this) {
			prevOwner = latestOwner;
			latestOwner = endPnt;
		}
		
		if(Debug.ENABLED)
			Debug.exclusiveLock(lock(), prevOwner, startPnt);
		
		if(prevOwner != null)
			prevOwner.addEdgeAndAdjust(startPnt, NONDETERMINISTIC);
	}
}
