package basic;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.quals.Requires;
import ch.ethz.intervals.quals.Subinterval;
import ch.ethz.intervals.quals.Happens;
import ch.ethz.intervals.quals.DefinesGhost;
import ch.ethz.intervals.quals.Creator;
import ch.ethz.intervals.quals.GuardedBy;

@DefinesGhost(type="ch.ethz.intervals.Interval")
@interface CircularGhost1 {
    public String value() default "";
}

@DefinesGhost(type="ch.ethz.intervals.Interval")
@interface CircularGhost2 {
    public String value() default "";
}

@CircularGhost1 @CircularGhost2
class CircularGhosts {
    
    @GuardedBy("CircularGhost1")
    int field1;
    
    @GuardedBy("CircularGhost2")
    int field2;

    void method(
        @CircularGhost1("x.CircularGhost2") 
        @CircularGhost2("x.CircularGhost1") 
        CircularGhosts x
    ) {
        x.field1 = 10;
        x.field2 = 10;
    }
    
}