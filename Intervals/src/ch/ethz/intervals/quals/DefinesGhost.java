package ch.ethz.intervals.quals;

import ch.ethz.intervals.guard.Guard;


/** 
    Meta-annotation that is used to designate a ghost annotation.
    The value {@link #ofClass()} defines the class of the objects which
    this ghost can be bound to.
*/
public @interface DefinesGhost {
	public Class<?> ofClass() default Guard.class;
}
