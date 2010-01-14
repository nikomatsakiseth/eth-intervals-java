package ch.ethz.intervals;

import java.util.Iterator;

import ch.ethz.intervals.TestInterval.ThrowExceptionTask;


final class SubintervalImpl<R> extends Interval {

	SubintervalTask<R> task;
	R result;

	SubintervalImpl(String name, Interval superInterval, Line line,
			SubintervalTask<R> task) 
	{
		super(name, superInterval, line, 0, 2);
		assert end.maskExceptions();
		this.task = task;
		
		// Build up a list of locks to acquire.  Be careful in 
		// case the user code throws an exception.		
		Lock[] locks;
		try {
			locks = task.locks();
		} catch(Throwable thr) {
			start.addPendingException(thr);
			locks = null;
		}		
		if(locks != null) {
			LockList lockList = null;
			// Preserver ordering of locks (may be sig. for deadlock):
			for(int i = locks.length - 1; i >= 0; i--)
				lockList = new LockList(this, locks[i], lockList);
			setLocksUnsync(lockList);
		}
	}

	@Override
	public String toString() {
		return task.toString();
	}

	@Override
	protected void run() {
		result = task.run(this);
	}

}
