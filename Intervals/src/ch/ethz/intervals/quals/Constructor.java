package ch.ethz.intervals.quals;

import ch.ethz.intervals.impl.IntervalImpl;

/** 
 * Designates the interval in which the object as a whole is being constructed. 
 * Added automatically to all classes.  By default, all types have the parameter
 * {@code \@Constructor(hbNow)}, but if you would like to override that default
 * you can manually use this annotation. */
@DefinesGhost(ofClass=IntervalImpl.class)
public @interface Constructor {
	public String value();
}
