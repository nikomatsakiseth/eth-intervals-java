package ch.ethz.intervals;


public class Lock
extends /*@Writer("this.constructor")*/ LockBase 
implements Guard
{
	public Lock() {
		super(false);
	}
	
	@Override
	public boolean isReadable() {
		return isWritable();
	}

	@Override
	public boolean isWritable() {
		Current current = Current.get();
		return (current.inter != null && current.inter.holdsLock(this));
	}

	@Override
	public String toString() {
		return String.format("Lock(%x)", System.identityHashCode(this));
	}
	
	/** @see DynamicGuard#isLockableBy(Point, Point) */
	boolean isLockableBy(Point mr, Point end) {
		return true;
	}

}
