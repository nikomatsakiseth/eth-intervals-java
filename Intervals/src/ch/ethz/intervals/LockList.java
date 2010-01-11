package ch.ethz.intervals;


/**
 * Small linked list object that used to record the locks we have acquired
 * and those that are still pending.
 */
class LockList 
extends LockBase
{
	final Lock lock;
	final boolean exclusive;
	LockList next;
	
	LockList(Lock lock, boolean exclusive, LockList next) {
		this.lock = lock;
		this.exclusive = exclusive;
		this.next = next;
	}

	@Override
	protected Lock lock() {
		return lock;
	}

}