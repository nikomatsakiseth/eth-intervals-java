package ch.ethz.intervals.quals;

/** 
    Used when a method effect variable designates multiple methods simultaneously.
    If the methods do not take the same number of parameters, then effects on 
    parameters will be ignored if they do not apply.  For example, in the class
    {@link Identity}, references to the parameter of {@code equals()} are ignored
    when computing the effects of {@code hashCode()}.
*/
public @interface MultipleMethodEffectParameter {
    public MethodEffectParameter[] methods();
}
