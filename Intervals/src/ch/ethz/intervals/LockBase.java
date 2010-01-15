package ch.ethz.intervals;

/**
 * A {@link LockBase} is a lockable object.  This includes both the
 * {@link Lock} instances exposed to the end user, and the {@link LockList}
 * instances that support recursive acquires.
 */
abstract class LockBase 
{
	/** True if we are locked. Access in synchronized only. */
	private boolean locked;	
	
	/** linked list queue of people waiting for the lock. 
	 *  Access in synchronized only. */
	private LockList firstPending = null, lastPending = null;
	
	public LockBase(boolean locked) {
		this.locked = locked;
	}

	/**
	 * If the lock is available, obtain it.  Otherwise,
	 * enqueue {@code acq}, adjusting its wait count
	 * appropriately.  Returns the number of
	 * arrivals that {@code acq.start} can expect
	 * (i.e., 0 if the lock was taken, 1 if not)
	 */
	final int tryAndEnqueue(LockList acq) {
		assert acq.nextPending == null && acq.acquiredLock == null;
		acq.acquiredLock = this;
		
		int result;
		synchronized(this) {
			if(!locked) { // Available.  Go to locked state.
				if(Debug.ENABLED)
					Debug.acquireLock(this, acq);
				
				locked = true;
				result = 0;
			} else { // Already locked.  Enqueue 'acq'.
				if(Debug.ENABLED)
					Debug.enqueueForLock(this, acq);
				
				if (firstPending == null) {
					firstPending = lastPending = acq;
				} else {
					lastPending.nextPending = acq;
					lastPending = acq;					
				}
				result = 1;
			} 
		}
		
		if(result == 0)
			acq.didAcquireLock(this);
		
		return result;
	}
	
	final void unlockThis() {		
		LockList newOwner;
		
		synchronized(this) {
			assert locked;
			
			// Check if anyone is pending.
			newOwner = firstPending;
			if(newOwner == null) { // No.  Clear locked flag.
				assert lastPending == null;
				locked = false;
				
				if(Debug.ENABLED)
					Debug.lockFree(this);
				
				return;
			} 
			
			// Yes.  Stay locked and remove newOwner from queue.
			if(newOwner == lastPending) { // They were last.  Queue now empty.
				assert newOwner.nextPending == null;
				firstPending = lastPending = null;
			} else { // More people waiting.
				firstPending = newOwner.nextPending;
			} 
		}
		
		if(Debug.ENABLED)
			Debug.dequeueForLock(this, newOwner);
		
		// If we get here, we dequeued newOwner.  Wake them up.
		newOwner.nextPending = null;
		newOwner.didAcquireLock(this);
	}

}
