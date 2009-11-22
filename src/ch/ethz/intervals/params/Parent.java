package ch.ethz.intervals.params;

import ch.ethz.intervals.quals.*;

/** 
    Refers to the guard/interval which contains the current guard/interval. 
*/
@ObjectParameter(covariant=false)
public @interface Parent {
    public String value() default "Parent";
}
