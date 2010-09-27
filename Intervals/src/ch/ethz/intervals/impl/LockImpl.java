package ch.ethz.intervals.impl;

import java.io.IOException;
import java.util.LinkedList;

import com.smallcultfollowing.lathos.Lathos;
import com.smallcultfollowing.lathos.Output;
import com.smallcultfollowing.lathos.Page;
import com.smallcultfollowing.lathos.PageContent;

import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.Lock;
import ch.ethz.intervals.RoInterval;
import ch.ethz.intervals.RoLock;
import ch.ethz.intervals.RoPoint;

public class LockImpl implements Lock, Page {
	
	private final String name;
	private LockRecord heldBy;
	private LinkedList<LockRecord> pending;

	public LockImpl(String name) {
		this.name = name;
		this.pending = null;
	}
	
	@Override
	public String toString() {
		if(name == null)
			return String.format("Lock[%x]", System.identityHashCode(this));
		else
			return name;
	}
	
	public static RuntimeException checkWritableImpl(RoLock self, RoPoint mr, RoInterval inter) {
		if(inter.locks(self, self))
			return null;
		
		return new IntervalException.LockNotHeld(self, self, inter);		
	}
	
	public static RuntimeException checkReadableImpl(RoLock self, RoPoint mr, RoInterval inter) {
		return checkWritableImpl(self, mr, inter);
	}
	
	public static RuntimeException ensuresFinalImpl(RoLock self, RoPoint mr, RoInterval current) {
		return new IntervalException.NeverFinal(self);
	}

	public static RuntimeException checkLockableImpl(RoLock self, RoPoint acq, RoInterval interval, RoLock lock) {
		if(lock == self)
			return null;
		
		return new IntervalException.CannotBeLockedBy(self, lock);
	}

	@Override
	public RuntimeException checkWritable(RoPoint mr, RoInterval inter) {
		return checkWritableImpl(this, mr, inter);
	}

	@Override
	public RuntimeException checkReadable(RoPoint mr, RoInterval current) {
		return checkReadableImpl(this, mr, current);
	}

	@Override
	public RuntimeException ensuresFinal(RoPoint mr, RoInterval current) {
		return ensuresFinalImpl(this, mr, current);
	}

	@Override
	public RuntimeException checkLockable(RoPoint acq, RoInterval interval, RoLock lock) {
		return checkLockableImpl(this, acq, interval, lock);
	}

	/** 
	 * Invoked by the acquisition point when lock should be
	 * acquired.  Notifies the acq. point once the lock has
	 * been successfully acquired (might be immediately). */
	void acquire(LockRecord record) {
		boolean successful;
		synchronized(this) {
			if(heldBy == null) {
				successful = true;
				heldBy = record;
			} else {
				successful = false;
				if(pending == null)
					pending = new LinkedList<LockRecord>();
				pending.add(record);
			}
		}
		
		if(successful) {
			if(Debug.ENABLED)
				Debug.debug.postLockAcquired(this, record);
			
			record.acquiredBy.didAcquireLock(record);
		} else {
			if(Debug.ENABLED)
				Debug.debug.postLockEnqueued(this, record);
		}
	}
	
	/**
	 * Invoked when lock is released. */
	void release(LockRecord from) {
		LockRecord awaken;
		synchronized(this) {
			assert heldBy == from;
			
			if(pending != null) {
				awaken = pending.removeFirst();
				if(pending.isEmpty())
					pending = null;
			} else {
				awaken = null;
				heldBy = null;
			}
		}
		
		if(awaken != null) {
			if(Debug.ENABLED)
				Debug.debug.postLockDequeued(this, awaken);
			
			awaken.acquiredBy.didAcquireLock(awaken);
		} else {
			if(Debug.ENABLED)
				Debug.debug.postLockFreed(this);
		}
	}

	@Override
	public void renderInPage(Output out) throws IOException {
		Lathos.reflectivePage(this, out);
	}

	@Override
	public void renderInLine(Output output) throws IOException {
		Lathos.renderInLine(this, output);
	}

	@Override
	public String getId() {
		return Lathos.defaultId(this);
	}

	@Override
	public Page getParent() {
		return Debug.debug;
	}

	@Override
	public void addContent(PageContent content) {
		throw new UnsupportedOperationException();
	}

}
