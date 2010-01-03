package ch.ethz.intervals.quals;

import java.lang.annotation.Target;

/**
 * Indicates that data guarded by the path {@link #value()}
 * should be readable by the interval {@link #by()} (default: {@code method}). 
 */
@Target({})
public @interface Readable {
	public String value();
	public String by() default "method";
}
