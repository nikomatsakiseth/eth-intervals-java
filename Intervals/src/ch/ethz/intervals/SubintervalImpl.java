package ch.ethz.intervals;



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
			// Build list in reverse order of the array:
			for(Lock lock : locks)
				lockList = new LockList(this, lock, lockList);
			setRevLocksUnsync(lockList);
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
