package ch.ethz.intervals.impl;

import static ch.ethz.intervals.util.ChunkList.NORMAL;
import ch.ethz.intervals.NotInRootIntervalException;
import ch.ethz.intervals.RethrownException;
import ch.ethz.intervals.guard.Guard;
import ch.ethz.intervals.mirror.Context;
import ch.ethz.intervals.mirror.Lock;
import ch.ethz.intervals.mirror.Task;
import ch.ethz.intervals.task.ResultTask;

public class ContextImpl implements Context {
	
	public static final Context intervals = new ContextImpl();
	
	/** Shared thread pool that executes tasks. */
	public static final ThreadPool POOL = new ThreadPool();

	private ContextImpl() {
		
	}
	
	@Override
	public Lock lock() {
		return null;
	}
	
	/**
	 * Convenience method for asserting that the current interval is readable.
	 * Intended to be used like: {@code assert checkReadable(guard);}
	 * 
	 * @param guard the guard to check for readability
	 * @returns true if {@code guard} is readable, and throws an exception otherwise.
	 */
	public boolean checkReadable(Guard guard) {
		Current current = Current.get();
		if(current.inter == null)
			throw new NotInRootIntervalException(); // later we could allow this maybe
		RuntimeException err = guard.checkReadable(current.mr, current.inter);
		if(err != null) throw err;
		return true;
	}
	
	/**
	 * Convenience method for asserting that the current interval is writable.
	 * 
	 * @see #checkReadable(Guard)
	 */
	public boolean checkWritable(Guard guard) {
		Current current = Current.get();
		if(current.inter == null)
			throw new NotInRootIntervalException(); // later we could allow this maybe
		RuntimeException err = guard.checkWritable(current.mr, current.inter);
		if(err != null) throw err;
		return true;
	}
	
	/**
	 * Creates a new interval which executes during the current interval.
	 * This interval will execute {@code task}.  This function does not
	 * return until the new interval has completed.
	 * 
	 * <b>Note:</b> Exceptions that occur in {@code task} are 
	 * wrapped in {@link RethrownException} and rethrown immediately.
	 * Exceptions never propagate to the current interval. */
	public void inline(final Task task)
	{		
		Current current = Current.get();
		
		InlineIntervalImpl subinterval = 
			new InlineIntervalImpl(current.inter, task);
		
		if(current.mr != null && current.mr != current.start())
			current.mr.addEdgeAfterOccurredWithoutException(
					subinterval.start, 
					NORMAL
			);
		
		if(Debug.ENABLED)
			Debug.subInterval(subinterval, task.toString());
		
		subinterval.execute();		
	}

	@Override
	public int getNumWorkers() {
		return POOL.numWorkers;
	}

	
}
