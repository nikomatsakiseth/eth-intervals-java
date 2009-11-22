package ch.ethz.intervals;


/**
 * Small linked list object that used to record the locks we have acquired
 * and those that are still pending.
 */
class LockList {
	final GuardImpl guard;
	final boolean exclusive;
	LockList next;
	
	LockList(GuardImpl guard, boolean exclusive, LockList next) {
		this.guard = guard;
		this.exclusive = exclusive;
		this.next = next;
	}

}