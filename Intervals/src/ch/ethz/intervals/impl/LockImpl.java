package ch.ethz.intervals.impl;

import java.io.IOException;
import java.util.LinkedList;

import com.smallcultfollowing.lathos.Lathos;
import com.smallcultfollowing.lathos.Output;
import com.smallcultfollowing.lathos.Page;
import com.smallcultfollowing.lathos.PageContent;

import ch.ethz.intervals.Condition;
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

	@Override
	public Condition condHeld() {
		return new Condition() {
			@Override
			public boolean isTrueFor(RoPoint mr, RoInterval current) {
				return current.locks(LockImpl.this, null);
			}
			
			@Override
			public String toString() {
				return String.format("locks(%s)", LockImpl.this);
			}
		};
	}

	@Override
	public Condition condReadableBy() {
		return condHeld();
	}

	@Override
	public Condition condWritableBy() {
		return condHeld();
	}

	@Override
	public Condition condFinal() {
		return Condition.FALSE;
	}

}
