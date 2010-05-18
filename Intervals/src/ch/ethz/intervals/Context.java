package ch.ethz.intervals;

import ch.ethz.intervals.guard.Guard;

/**
 * A Context embodies one particular intervals runtime.  It 
 * allows one to create new locks and intervals.  It also
 * tracks the currently executing interval.
 * 
 * The result of passing objects (intervals, locks, points) of one context
 * as parameters to methods of objects from another context 
 * is undefined. 
 */
public interface Context {
	
	/** 
	 * An estimate of the number of worker threads, or put 
	 * another way, amount of available parallelism. */
	public int getNumWorkers();
	
	/**
	 * Creates a new lock object. */
	public Lock lock();
	
	/**
	 * Convenience method for asserting that the current interval is readable.
	 * Intended to be used like: {@code assert checkReadable(guard);}
	 * 
	 * @param guard the guard to check for readability
	 * @returns true if {@code guard} is readable, and throws 
	 * an exception otherwise. */
	public boolean checkReadable(Guard guard);
	
	/**
	 * Convenience method for asserting that the current interval is writable.
	 * 
	 * @see #checkReadable(Guard) */
	public boolean checkWritable(Guard guard);
		
	/**
	 * Creates a new interval which executes during the current interval.
	 * This interval will execute {@code task}.  This function does not
	 * return until the new interval has completed.
	 * 
	 * <b>Note:</b> Exceptions that occur in {@code task} are 
	 * wrapped in {@link RethrownException} and rethrown immediately.
	 * Exceptions never propagate to the current interval. */
	public void inline(Task task);
	
}
