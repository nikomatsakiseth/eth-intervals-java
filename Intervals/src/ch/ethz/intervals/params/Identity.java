package ch.ethz.intervals.params;

import ch.ethz.intervals.quals.ObjectParameter;

/** 
    This parameter is defined by default on {@link Object}.  
    It indicates the partition(s) which contain the object's
    identity; in other words, those used by {@link Object#hashCode()}
    or {@link Object#equals(Object)}.
*/
@ObjectParameter(covariant=true)
public @interface Identity {
    public String value() default "";
}
