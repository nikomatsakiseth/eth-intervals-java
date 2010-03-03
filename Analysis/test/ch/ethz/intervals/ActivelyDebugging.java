package ch.ethz.intervals;

import java.lang.annotation.*; 

@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD, ElementType.TYPE})
public @interface ActivelyDebugging {}