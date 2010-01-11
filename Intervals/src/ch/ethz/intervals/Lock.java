package ch.ethz.intervals;


public final class Lock
extends /*@Writer("this.constructor")*/ LockBase 
implements Guard
{
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
	protected Lock lock() {
		return this;
	}

}
