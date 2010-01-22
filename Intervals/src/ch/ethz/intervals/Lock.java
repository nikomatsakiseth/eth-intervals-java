package ch.ethz.intervals;


public class Lock
extends /*@Writer("this.constructor")*/ LockBase 
implements Guard, LockMirror
{
	private final String name;
	
	public Lock() {
		super(false);
		this.name = null;
	}
	
	public Lock(String name) {
		super(false);
		this.name = name;
	}
	
	@Override
	public boolean checkReadable() {
		return checkWritable();
	}

	@Override
	public boolean checkWritable() {
		Current current = Current.get();
		if(current.inter == null || !current.inter.locks(this))
			throw new IntervalException.LockNotHeld(this, current.inter);
		return true;
	}

	@Override
	public String toString() {
		if(name != null)
			return name;
		return String.format("Lock(%x)", System.identityHashCode(this));
	}
	
	/** @see DynamicGuard#checkLockableByReturningException(Interval) */
	IntervalException checkLockableByReturningException(Interval inter) {
		return null;
	}

}
