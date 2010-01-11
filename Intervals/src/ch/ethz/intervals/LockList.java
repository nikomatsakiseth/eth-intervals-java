package ch.ethz.intervals;


/**
 * LockLists serve many purposes related to acquiring locks.
 * 
 * Before the lock is acquired, the {@link LockList} simply serves as a 
 * little list to track which locks an interval should acquire befor
 * activating.
 * 
 * When {@link #lock} is acquired (or while we wait to acquire it),
 * the {@link #nextPending} field is used by {@link #lock} to 
 * track any other intervals contending for the lock that are forced to queue.
 * 
 * In addition, because a {@link LockList} extends {@link LockBase}, they are
 * lockable objects.  This is to support recursive acquires: any subinterval
 * acquiring {@link #lock} actually contends for locking {@code this}.  
 * Because LockLists begin in a locked state, recursive acquires will initially
 * block until the interval's {@link Interval#run()} method finishes, which
 * causes all of its {@link LockList} instances to be unlocked and hence
 * available for recursive acquires.
 */
class LockList 
extends LockBase
{
	/** The interval which will be acquiring lock */
	final Interval inter;
	
	/** User-specified lock to acquire */
	final Lock lock;
	
	/** Next lock to acquire */
	LockList next;
	
	/** The actual lockable object we acquired. May be {@link #lock} 
	 *  but in the case of a recursive acquire may be something else. 
	 *  Written automatically by {@link LockBase#tryAndEnqueue(LockList)} */
	LockBase acquiredLock;
	
	/** Owned by the {@link LockBase}, used to track pending candidates */
	LockList nextPending;
	
	LockList(Interval inter, Lock lock, LockList next) {
		super(true);
		this.inter = inter;
		this.lock = lock;
		this.next = next;
	}

	@Override
	protected Lock lock() {
		return lock;
	}

	public void unlockAcquiredLock() {
		acquiredLock.unlockThis();
		acquiredLock = null;
	}
	
	public String toString() {
		return String.format("LockList(%x,%s,%s)", System.identityHashCode(this), inter, inter.start);
	}

}