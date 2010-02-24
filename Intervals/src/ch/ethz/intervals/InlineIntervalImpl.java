package ch.ethz.intervals;

import java.util.Set;




final class InlineIntervalImpl<R> extends Interval {

	InlineTask<R> task;
	private R result;
	private Set<Throwable> errors;

	InlineIntervalImpl(String name, Interval superInterval, InlineTask<R> task) 
	{
		super(name, superInterval, Point.FLAG_SYNCHRONOUS, 0, 2);
		this.task = task;
		
		// Build up a list of locks to acquire.  Be careful in 
		// case the user code throws an exception.		
		Lock[] locks;
		try {
			locks = task.locks();
		} catch(Throwable thr) {
			addVertExceptionUnsyc(thr);
			locks = null;
		}		
		if(locks != null) {
			LockList lockList = null;
			// Build list in reverse order of the array:
			for(Lock lock : locks)
				lockList = new LockList(this, lock, null, lockList);
			setRevLocksUnsync(lockList);
		}
	}
	
	@Override
	protected Set<Throwable> catchErrors(Set<Throwable> errors) {
		this.errors = errors;
		return null;
	}

	@Override
	public String toString() {
		return task.toString();
	}

	@Override
	protected void run() {
		result = task.run(this);
	}
	
	R readResultOrRethrowErrors() {
		if(errors != null)
			throw new RethrownException(errors);
		return result;
	}

}
