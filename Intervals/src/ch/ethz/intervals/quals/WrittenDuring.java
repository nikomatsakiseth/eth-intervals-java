package ch.ethz.intervals.quals;

import ch.ethz.intervals.Interval;

/**
 * Used to indicate the {@link Interval} when a field
 * will be written or the {@link AddLock}.  Its value should be a path
 * like {@code "a.b.c"} where {@code a}, {@code b},
 * etc are field or ghosts of the current class.
 */
public @interface WrittenDuring {
	public String value();
}