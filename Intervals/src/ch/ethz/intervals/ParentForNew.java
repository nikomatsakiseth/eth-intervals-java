package ch.ethz.intervals;

import ch.ethz.intervals.quals.DefinesGhost;

/** Ghost field designating the parent interval specified by a {@link Dependency} */
@DefinesGhost(ofClass=Interval.class, useByDefault=false)
public @interface ParentForNew {
	public String value() default "";
}
