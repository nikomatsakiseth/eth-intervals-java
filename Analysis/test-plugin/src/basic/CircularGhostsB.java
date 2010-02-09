package basic;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.quals.Requires;
import ch.ethz.intervals.quals.Subinterval;
import ch.ethz.intervals.quals.Happens;
import ch.ethz.intervals.quals.DefinesGhost;
import ch.ethz.intervals.quals.Creator;
import ch.ethz.intervals.quals.GuardedBy;

@DefinesGhost(type="ch.ethz.intervals.Interval")
@interface CircularGhostB1 {
    public String value() default "";
}

@DefinesGhost(type="ch.ethz.intervals.Interval")
@interface CircularGhostB2 {
    public String value() default "";
}

@CircularGhostB1 @CircularGhostB2
class CircularGhostsB {
    
    @GuardedBy("CircularGhostB1")
    int field1;
    
    @GuardedBy("CircularGhostB2")
    int field2;

    void method(
        @CircularGhostB1("hb x.CircularGhostB2") 
        @CircularGhostB2("hb x.CircularGhostB1") 
        CircularGhostsB x
    ) {
        int i = x.field1; // ERROR Interval "x.`basic.CircularGhostB1`" is not readable because the current interval may not happen after it.
        int j = x.field2; // ERROR Interval "x.`basic.CircularGhostB2`" is not readable because the current interval may not happen after it.
    }
    
}