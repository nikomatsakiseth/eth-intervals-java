package ch.ethz.intervals;


public class Lock
extends /*@Writer("this.constructor")*/ LockBase 
implements Guard
{
	public Lock() {
		super(false);
	}
	
	@Override
	public boolean checkReadable() {
		return checkWritable();
	}

	@Override
	public boolean checkWritable() {
		Current current = Current.get();
		if(current.inter == null || !current.inter.holdsLock(this))
			throw new IntervalException.LockNotHeld(this, current.inter);
		return true;
	}

	@Override
	public String toString() {
		return String.format("Lock(%x)", System.identityHashCode(this));
	}
	
	/** @see DynamicGuard#checkLockableByReturningException(Interval) */
	IntervalException checkLockableByReturningException(Interval inter) {
		return null;
	}

}
