package ch.ethz.intervals.impl;

import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.guard.Guard;
import ch.ethz.intervals.mirror.Interval;
import ch.ethz.intervals.mirror.Lock;
import ch.ethz.intervals.mirror.Point;


public class LockImpl
extends /*@Writer("this.constructor")*/ LockBase 
implements Guard, Lock
{
	private final String name;
	
	public LockImpl() {
		super(false);
		this.name = null;
	}
	
	public LockImpl(String name) {
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
	public IntervalException checkLockable(Interval interval, Lock lock) {
		if(lock != this)
			return new IntervalException.CannotBeLockedBy(this, lock);
		return null;
	}

	@Override
	public IntervalException checkReadable(Point mr, Interval inter) {
		return checkWritable(mr, inter);
	}

	@Override
	public IntervalException checkWritable(Point mr, Interval inter) {
		if(!inter.locks(this))
			return new IntervalException.LockNotHeld(this, inter);
		return null;
	}

}
