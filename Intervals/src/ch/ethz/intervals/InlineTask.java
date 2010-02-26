package ch.ethz.intervals;

public abstract class InlineTask<R> {
	/** Invoked before the inline interval actually begins. 
	 *  Can be used to add locks or <i>happens before</i> relationships. */
	public void init(Interval subinterval) {}
	
	/** The method to be executed by the inline subinterval. */
	public abstract R run(Interval subinterval);
}
