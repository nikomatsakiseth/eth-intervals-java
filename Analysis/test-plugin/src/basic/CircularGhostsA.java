package basic;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.quals.DefinesGhost;
import ch.ethz.intervals.quals.GuardedBy;

@DefinesGhost(ofClass=Interval.class)
@interface CircularGhostA1 {
    public String value() default "";
}

@DefinesGhost(ofClass=Interval.class)
@interface CircularGhostA2 {
    public String value() default "";
}

@CircularGhostA1 @CircularGhostA2
class CircularGhostsA {
    
    @GuardedBy("CircularGhostA1")
    int field1;
    
    @GuardedBy("CircularGhostA2")
    int field2;

    void method(
        @CircularGhostA1("x.CircularGhostA2") 
        @CircularGhostA2("x.CircularGhostA1") 
        CircularGhostsA x
    ) {
        x.field1 = 10; // ERROR Interval "x.(basic.CircularGhostA2)" is not writable because it may not be the current interval.
        x.field2 = 10; // ERROR Interval "x.(basic.CircularGhostA1)" is not writable because it may not be the current interval.
    }
    
}