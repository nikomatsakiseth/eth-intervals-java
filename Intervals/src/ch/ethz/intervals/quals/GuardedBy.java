package ch.ethz.intervals.quals;

import java.lang.annotation.ElementType;
import java.lang.annotation.Target;

import ch.ethz.intervals.Guard;

/**
 * Specifies a path to the <b>guard</b> for a field.
 * A guard is an object that defines when a field may be
 * read or written so as to prevent data races.  The
 * value specified by this annotation must be of type
 * {@link Guard}.
 * 
 * <p>The path to the guard may either lead to a real field
 * or a ghost parameter (see {@link DefinesGhost}).
 * 
 * @see Guard
 */
@Target(ElementType.FIELD)
public @interface GuardedBy {
	/** The path to this field's guard.  A path should
	 *  be a dot-separated list of fields like {@code this.a.b.c}. 
	 *  Each field may either be a real field or a ghost parameter.
	 *  @see DefinesGhost */
	public String value();
}
