package ch.ethz.intervals.impl;

import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.Lock;
import ch.ethz.intervals.RoInterval;
import ch.ethz.intervals.RoLock;
import ch.ethz.intervals.RoPoint;
import ch.ethz.intervals.guard.Guard;

public class LockImpl
extends /*@Writer("this.constructor")*/ LockBase 
implements Guard, Lock
{
	private final String name;
	
	public LockImpl() {
		this(null);
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
	public IntervalException checkLockable(RoInterval interval, RoLock lock) {
		if(lock != this)
			return new IntervalException.CannotBeLockedBy(this, lock);
		return null;
	}

	@Override
	public IntervalException checkReadable(RoPoint mr, RoInterval inter) {
		return checkWritable(mr, inter);
	}

	@Override
	public IntervalException checkWritable(RoPoint mr, RoInterval inter) {
		if(!inter.locks(this))
			return new IntervalException.LockNotHeld(this, inter);
		return null;
	}

}
