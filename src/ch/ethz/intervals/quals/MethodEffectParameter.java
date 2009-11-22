package ch.ethz.intervals.quals;

/** 
    Meta-annotation that designates a method effect parameter. 
    The annotated annotations should have a single field,
    <code>String value();</code>.  
*/
public @interface MethodEffectParameter {
	public String name();
	public Class[] argumentTypes();
}
