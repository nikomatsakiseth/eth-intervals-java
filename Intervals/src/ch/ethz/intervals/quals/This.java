package ch.ethz.intervals.quals;

/** 
    The type annotation @This(...) specifies a path
    that is the same as the "this" pointer.  
    Used to link an object parameter or return value
    with an actual parameter.
    
    As an example, see {@link ch.ethz.intervals.GuardImpl#parentGuard()},
    which returns the parent guard (always the same as the
    parameter {@link ch.ethz.intervals.params.Parent}).
*/
public @interface This {
    public String value() default "This";
}
