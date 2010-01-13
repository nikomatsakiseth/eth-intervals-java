package ch.ethz.intervals.quals;


/**
 * 
 */
@DefinesGhost
public @interface Creator {
	public String value() default "";
}
