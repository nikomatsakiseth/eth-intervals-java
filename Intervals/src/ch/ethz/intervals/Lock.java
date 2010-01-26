package ch.ethz.intervals;

import ch.ethz.intervals.guard.Guard;
import ch.ethz.intervals.mirror.IntervalMirror;
import ch.ethz.intervals.mirror.LockMirror;
import ch.ethz.intervals.mirror.PointMirror;


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
	public String toString() {
		if(name != null)
			return name;
		return String.format("Lock(%x)", System.identityHashCode(this));
	}
	
	@Override
	public IntervalException checkLockable(IntervalMirror interval, LockMirror lock) {
		if(lock != this)
			return new IntervalException.CannotBeLockedBy(this, lock);
		return null;
	}

	@Override
	public IntervalException checkReadable(PointMirror mr, IntervalMirror inter) {
		return checkWritable(mr, inter);
	}

	@Override
	public IntervalException checkWritable(PointMirror mr, IntervalMirror inter) {
		if(!inter.locks(this))
			return new IntervalException.LockNotHeld(this, inter);
		return null;
	}

}
