package ch.ethz.intervals.quals;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

import ch.ethz.intervals.guard.Guard;


/**
 * Designates the guard which authorizes writes to the object.
 * Declared on the class {@link Object} and therefore inherited
 * by all objects.    
 */
@DefinesGhost(ofClass=Guard.class, useByDefault=true)
public @interface Creator {
	public String value() default "";
}
