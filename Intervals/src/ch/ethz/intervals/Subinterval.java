package ch.ethz.intervals;

import ch.ethz.intervals.quals.DefinesGhost;

@DefinesGhost(ofClass=Interval.class)
public @interface Subinterval {
	public String value() default "";
}
