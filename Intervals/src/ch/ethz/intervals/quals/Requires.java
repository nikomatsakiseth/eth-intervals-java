package ch.ethz.intervals.quals;

import java.lang.annotation.ElementType;
import java.lang.annotation.Target;

/**
 * Specifies the requirements for a METHOD to be executed.
 */
@Target(ElementType.METHOD)
public @interface Requires {
	Subinterval[] subinterval() default {};
	Readable[] readable() default {};
	Writable[] writable() default {};
	Happens[] happens() default {};
	HoldsLock[] holdsLock() default {};	
}
