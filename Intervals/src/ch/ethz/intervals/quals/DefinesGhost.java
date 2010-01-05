package ch.ethz.intervals.quals;


/** 
    Meta-annotation that is used to designate a ghost annotation.
    The value {@link #type()} defines the type of the object that this
    ghost can be linked to.
*/
public @interface DefinesGhost {
	public String type() default "ch.ethz.intervals.Guard";
}
