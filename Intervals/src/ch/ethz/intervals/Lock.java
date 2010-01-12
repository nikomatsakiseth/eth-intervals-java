package ch.ethz.intervals;


public class Lock
extends /*@Writer("this.constructor")*/ LockBase 
implements Guard
{
	Lock() {
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
	
	/** Invoked whenever an interval acquires this lock, either initially or
	 *  recursively.  Used to drive the dynamic race detector. */
	void didLock(Interval inter) {}
	
}
