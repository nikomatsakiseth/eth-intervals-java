package ch.ethz.intervals.params;

import ch.ethz.intervals.quals.*;

/** 
    The Locked guard is defined on the root package and therefore in 
    scope for all classes.  It contains data which is only accessed
    under lock.
*/
@ObjectParameter
public @interface Locked {
    public String value() default "Locked";
}
