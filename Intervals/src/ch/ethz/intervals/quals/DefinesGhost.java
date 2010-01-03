package ch.ethz.intervals.quals;

import ch.ethz.intervals.Guard;

/** 
    Meta-annotation that designates an object parameter. 
    Can only be applied to annotations with a single
    field, <code>String value();</code>
*/
public @interface DefinesGhost {
	public Class<?> cls() default Guard.class;
}
