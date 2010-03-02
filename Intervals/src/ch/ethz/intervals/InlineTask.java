package ch.ethz.intervals;

import ch.ethz.intervals.quals.Is;
import ch.ethz.intervals.quals.Requires;

@Subinterval
public abstract class InlineTask<R> {
	/** Invoked before the inline interval actually begins. 
	 *  Can be used to add locks or <i>happens before</i> relationships. */
	public void init(@Is("Subinterval") Interval subinterval) {}
	
	/** The method to be executed by the inline subinterval. */
	@Requires("method suspends Subinterval")
	public abstract R run(@Is("Subinterval") Interval subinterval);
}
