package ch.ethz.intervals.impl;

import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.Lock;
import ch.ethz.intervals.RoInterval;
import ch.ethz.intervals.RoLock;
import ch.ethz.intervals.RoPoint;

public class LockImpl
extends /*@Writer("this.constructor")*/ LockBase 
implements Lock
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
	
	public static IntervalException checkReadableImpl(RoLock self, RoPoint mr, RoInterval inter) {
		return checkWritableImpl(self, mr, inter);
	}
	
	public static IntervalException checkWritableImpl(RoLock self, RoPoint mr, RoInterval inter) {
		if(inter.locks(self))
			return null;
		
		return new IntervalException.LockNotHeld(self, inter);
	}
	
	public static IntervalException checkLockableImpl(RoLock self, RoInterval interval, RoLock lock) {
		if(!lock.equals(self))
			return new IntervalException.CannotBeLockedBy(self, lock);
		
		return null;
	}
	
	public static IntervalException ensuresFinalImpl(RoLock self, RoPoint mr, RoInterval inter) {
		return new IntervalException.NeverFinal(self);		
	}
	
	@Override
	public IntervalException checkReadable(RoPoint mr, RoInterval inter) {
		return checkReadableImpl(this, mr, inter);
	}

	@Override
	public RuntimeException checkWritable(RoPoint mr, RoInterval inter) {
		return checkWritableImpl(this, mr, inter);
	}
	
	@Override
	public IntervalException checkLockable(RoInterval interval, RoLock lock) {
		return checkLockableImpl(this, interval, lock);		
	}

	@Override
	public RuntimeException ensuresFinal(RoPoint mr, RoInterval inter) {
		return ensuresFinalImpl(this, mr, inter);
	}

}
