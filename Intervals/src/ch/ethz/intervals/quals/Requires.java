package ch.ethz.intervals.quals;

import java.lang.annotation.ElementType;
import java.lang.annotation.Target;

/**
 * Specifies additional requirements for a METHOD to be executed beyond
 * those contained in the {@link BaseRequirements}.
 * 
 * Requirements are strings and can be of the following forms:
 * <ul>
 * <li> <tt>[guard] readableBy [interval]</tt>
 * <li> <tt>[guard] writableBy [interval]</tt>
 * <li> <tt>[guard] immutableIn [interval]</tt>
 * <li> <tt>[interval/point] hb [interval/point]</tt>
 * <li> <tt>[interval] suspends [interval]</tt>
 * </ul>
 */
@Target(ElementType.METHOD)
public @interface Requires {
	public String[] value() default { };
}
