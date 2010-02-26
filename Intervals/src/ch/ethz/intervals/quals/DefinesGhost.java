package ch.ethz.intervals.quals;

import ch.ethz.intervals.guard.Guard;


/** Meta-annotation that is used to designate a ghost annotation. */
public @interface DefinesGhost {
	/** The class of objects that this ghost can be bound to. */
	public Class<?> ofClass() default Guard.class;
	
	/** If true, then any types within a class that do not bind this
	 *  ghost will use {@code this.Ghost} by default. */
	public boolean useByDefault() default true;
}
