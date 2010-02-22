package ch.ethz.intervals.quals;

import java.lang.annotation.ElementType;
import java.lang.annotation.Target;

/**
 * Specifies the requirements for a METHOD to be executed.
 * 
 * Requirements are strings and can be of the following forms:
 * <ul>
 * <li> <tt>[guard] readableBy [interval]</tt>
 * <li> <tt>[guard] writableBy [interval]</tt>
 * <li> <tt>[guard] immutableIn [interval]</tt>
 * <li> <tt>[interval/point] hb [interval/point]</tt>
 * <li> <tt>[interval] subintervalOf [interval]</tt>
 * </ul>
 */
@Target(ElementType.METHOD)
public @interface Requires {
	/**
	 * Requirements that are only applied to constructor.
	 */
	public String[] constructor() default { "method subintervalOf this.Constructor" };

	/**
	 * Requirements that are only applied to regular instance methods.
	 */
	public String[] instanceMethod() default { 
		"this.Constructor hb method",
		"this.Creator writableBy method"
	};

	/**
	 * Requirements applied to any method.
	 */
	public String[] value() default { };
}
