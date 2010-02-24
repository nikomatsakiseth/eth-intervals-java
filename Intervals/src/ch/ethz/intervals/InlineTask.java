package ch.ethz.intervals;

public abstract class InlineTask<R> {
	/** Returns an array of locks to be acquired by the inline subinterval 
	 *  when it executes. The locks will be acquired in the order 
	 *  given.  May return null if no locks should be acquired. */
	public Lock[] locks() {
		return null;
	}

	/** The method to be executed by the inline subinterval. */
	public abstract R run(Interval inlineInterval);
}
