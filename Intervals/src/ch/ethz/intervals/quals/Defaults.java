package ch.ethz.intervals.quals;

public @interface Defaults {
	Requires requires() default @Requires;
}
