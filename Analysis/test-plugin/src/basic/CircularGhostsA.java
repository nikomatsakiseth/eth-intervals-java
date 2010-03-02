package basic;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.quals.DefinesGhost;
import ch.ethz.intervals.quals.GuardedBy;
import ch.ethz.intervals.quals.Requires;

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

    void methodNoAccess(
        @CircularGhostA1("x.CircularGhostA2") 
        @CircularGhostA2("x.CircularGhostA1") 
        final CircularGhostsA x
    ) {
        x.field1 = 10; // ERROR Guard "x.(basic.CircularGhostA1)" is not writable.
        x.field2 = 10; // ERROR Guard "x.(basic.CircularGhostA2)" is not writable.
    }
    
    // Note that the type checker knows that both A1 and A2 are the same object,
    // so if one is writable both are:
    @Requires("x.CircularGhostA1 writableBy method")
    void methodAccess(
        @CircularGhostA1("x.CircularGhostA2") 
        @CircularGhostA2("x.CircularGhostA1") 
        final CircularGhostsA x
    ) {
        x.field1 = 10;
        x.field2 = 10;
    }
        
}