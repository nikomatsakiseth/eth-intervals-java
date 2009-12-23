package ch.ethz.intervals.params;

import ch.ethz.intervals.quals.DefinesGhost;

@DefinesGhost
public @interface writer {
	public String value() default "";
}
