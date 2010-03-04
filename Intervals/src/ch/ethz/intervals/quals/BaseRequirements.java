package ch.ethz.intervals.quals;

/**
 * Specifies a base set of requirements that are applied constructors 
 * and methods by default.  This annotation can be applied to a method
 * to override the defaults at that particular point, or to a class or
 * package to override its members.
 */
public @interface BaseRequirements {
	/** Base requirements for constructors. */
	public String[] constructor() default { "method suspends this.Constructor" };

	/** Base requirements for instance methods. */
	public String[] instanceMethod() default { 
		"this.Constructor hb method",
		"this.Creator writableBy method"
	};
}
