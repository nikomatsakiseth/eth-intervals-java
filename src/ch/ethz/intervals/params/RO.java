package ch.ethz.intervals.params;

import ch.ethz.intervals.quals.*;

/** 
    The RO guard is defined on the root package and therefore in 
    scope for all classes.  It is used for data that is never written,
    and is the default guard for {@code final} fields.
*/
@ObjectParameter
public @interface RO {
    public String value() default "Locked";
}
