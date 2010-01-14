package ch.ethz.intervals;

public abstract class SubintervalTask<R> {
	/** Returns an array of locks to be acquired by the subinterval 
	 *  when it executes. The locks will be acquired in the order 
	 *  given.  May return null if no locks should be acquired. */
	public Lock[] locks() {
		return null;
	}

	/** The method to be executed by the subinterval. */
	public abstract R run(Interval subinterval);
}
