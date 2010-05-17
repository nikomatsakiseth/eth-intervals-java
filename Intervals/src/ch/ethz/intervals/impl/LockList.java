package ch.ethz.intervals.impl;

import ch.ethz.intervals.guard.Guard;


/**
 * LockLists serve many purposes related to acquiring locks.
 * 
 * Before the lock is acquired, the {@link LockList} simply serves as a 
 * little list to track which locks an interval should acquire befor
 * activating.
 * 
 * When {@link #lockImpl} is acquired (or while we wait to acquire it),
 * the {@link #nextPending} field is used by {@link #lockImpl} to 
 * track any other intervals contending for the lock that are forced to queue.
 * 
 * In addition, because a {@link LockList} extends {@link LockBase}, they are
 * lockable objects.  This is to support recursive acquires: any subinterval
 * acquiring {@link #lockImpl} actually contends for locking {@code this}.  
 */
class LockList 
extends LockBase
{
	/** The interval which will be acquiring lock */
	final IntervalImpl inter;
	
	/** User-specified lock to acquire */
	final LockImpl lockImpl;
	
	/** Guard being protected by {@link #lockImpl}, if any. */
	final Guard guard;
	
	/** Next lock to acquire */
	LockList next;
	
	/** The actual lockable object we acquired. May be {@link #lockImpl} 
	 *  but in the case of a recursive acquire may be something else. 
	 *  Written automatically by {@link LockBase#tryAndEnqueue(LockList)} */
	LockBase acquiredLock;
	
	/** Owned by the {@link LockBase}, used to track pending candidates */
	LockList nextPending;
	
	LockList(IntervalImpl inter, LockImpl lockImpl, Guard guard, LockList next) {
		super(false);
		this.inter = inter;
		this.lockImpl = lockImpl;
		this.guard = guard;
		this.next = next;
	}
	
	/**
	 * Invoked when we acquire the lock.  
	 * @param acquiredLock the lock we acquired, see {@link #acquiredLock}
	 */	
	void didAcquireLock(LockBase acquiredLock) {
		this.acquiredLock = acquiredLock;
		inter.didAcquireLock(this);
	}

	public void unlockAcquiredLock() {
		acquiredLock.unlockThis();
		acquiredLock = null;
	}
	
	public String toString() {
		return String.format("LockList(%x,%s,%s)", System.identityHashCode(this), inter, inter.start);
	}

}