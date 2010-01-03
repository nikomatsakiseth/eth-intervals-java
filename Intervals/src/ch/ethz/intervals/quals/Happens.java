package ch.ethz.intervals.quals;

import java.lang.annotation.Target;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Point;

/** 
 * Indicates that the {@link Interval}(s) or {@link Point}(s) {@link #before()} <em>happen before</em>
 * the {@link Interval}(s) or {@link Point}(s) {@link #after()}. 
 */
@Target({})
public @interface Happens {
	public String[] before();
	public String[] after();
}
