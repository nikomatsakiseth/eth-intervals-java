package ch.ethz.intervals.params;

import ch.ethz.intervals.quals.DefinesGhost;

/**
 * 
 */
@DefinesGhost
public @interface creator {
	public String value() default "";
}
