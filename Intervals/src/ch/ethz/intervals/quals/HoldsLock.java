package ch.ethz.intervals.quals;

import java.lang.annotation.Target;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Lock;

/** 
 * Indicates that the {@link Interval}(s) {@link #interval} hold
 * the {@link Lock}(s) {@link #lock()}. 
 */
@Target({})
public @interface HoldsLock {
	public String[] interval() default "method";
	public String[] lock();
}
