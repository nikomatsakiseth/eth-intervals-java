package ch.ethz.intervals.quals;

import java.lang.annotation.ElementType;
import java.lang.annotation.Target;


/**
 * Equivalent to {@link GuardedBy}.  Intended for use with interval
 * values; the resulting class definition is more readable.
 */
@Target(ElementType.FIELD)
public @interface WrittenDuring {
	public String value();
}