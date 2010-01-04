package ch.ethz.intervals.quals;

import java.lang.annotation.Target;

/**
 * Indicates that one interval (usually the interval corresponding
 * to the current method) must be a subinterval of another.
 */
@Target({})
public @interface Subinterval {
	/** Path(s) to the subinterval.  Generally the default
	 *  of {@code method} suffices. */
	public String[] subinterval() default "method";
	
	/** Path(s) to the superinterval. */
	public String[] of();
}
