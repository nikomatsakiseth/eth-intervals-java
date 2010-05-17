package ch.ethz.intervals.mirror;

import java.util.Set;

/** 
 * A task defines the behavior and error-catching behavior
 * of an interval. */
public interface Task {
	
	/** Returns the name of this task. */
	public String getName();
	
	/** 
	 * Invoked when a new interval is created with
	 * this as its task. Gives the task a chance
	 * to add locks or happens-before dependencies. */
	public void attachedTo(Interval inter);	
	
	/**
	 * Defines the behavior of the interval.  Must be
	 * overridden. Non-blocking child intervals of only 
	 * execute once this method has returned.
	 * 
	 * <p>Executed by the scheduler when {@code this}
	 * is the current interval.  <b>Do not invoke manually.</b> */
	public void run(Interval current) throws Exception;
	
	/** Invoked if an error occurs in this interval or in a child.  Gives the
	 *  interval an opportunity to filter and handle errors before propagating
	 *  them onwards.  If you return {@code null} or an empty set, the errors are 
	 *  considered handled and so the interval is considered to terminate without error.
	 *  Otherwise, the returned errors will be propagated to {@link Interval#getParent()},
	 *  and any successors of {@code this} will not execute.
	 *   
	 *  @param errors An immutable set of errors which occurred.
	 *  @returns the set of errors to propagate forward, or {@code null} for none. */
	public Set<? extends Throwable> catchErrors(Set<Throwable> errors);
	
}
