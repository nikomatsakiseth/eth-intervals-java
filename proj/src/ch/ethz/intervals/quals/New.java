package ch.ethz.intervals.quals;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/** 
    When used on a field, specifies that all assignments to that field are from newly
    created objects.  When used on a method, specifies that the method always returns
    a newly created object.  For fields, typically used in conjunction with the
    {@code final} modifier.
*/
@Target({ElementType.FIELD, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
public @interface New {
    /** If checked is {@code false}, then the static checker will not 
        try to verify that {@code New} is correctly used. */
    public boolean checked() default true;
}
