package ch.ethz.intervals.params;

/** 
    Special class used to model Java's static data. All object parameters of static are always in
    scope, even in static fields and methods. Other than serving as a central location to find the
    globally available object parameters, this class does not have much interest to users,
    and need never be referenced in user code.
*/
@RO
public class Static {
    private Static() {} // Should never be instantiated.
}
