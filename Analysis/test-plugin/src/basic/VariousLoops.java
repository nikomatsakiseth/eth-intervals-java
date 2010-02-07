package basic;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.quals.Creator;
import ch.ethz.intervals.quals.Requires;
import ch.ethz.intervals.quals.Subinterval;
import ch.ethz.intervals.quals.GuardedBy;

public class VariousLoops {

    final Interval x;
    final Interval y;
	@GuardedBy("x") int xInt;
	@GuardedBy("y") int yInt;
	
	VariousLoops(Interval x, Interval y) {
	    this.x = x;
	    this.y = y;
	}
	
	@Requires(subinterval=@Subinterval(of="x"))	
	protected void oldStyleForLoop() {
	    for(
	        xInt = 0;
	        xInt < 10;
	        xInt++
	    );
	    
	    for(
	        yInt = 0;       // ERROR Interval "this.y" is not writable because it may not be the current interval.
	        yInt < 10;      // ERROR Interval "this.y" is not readable because the current interval may not happen after it.
	        yInt++          // ERROR Interval "this.y" is not writable because it may not be the current interval.
	    );
	}

	@Requires(subinterval=@Subinterval(of="x"))	
	protected void whileLoop() {
	    xInt = 0;
	    while(xInt < 10) 
	    {
	        xInt++;
	    }
	    
	    yInt = 0;           // ERROR Interval "this.y" is not writable because it may not be the current interval.
	    while(yInt < 10)    // ERROR Interval "this.y" is not readable because the current interval may not happen after it.
        {
	        yInt++;         // ERROR Interval "this.y" is not writable because it may not be the current interval.
	    }
	}
	
	@Requires(subinterval=@Subinterval(of="x"))	
	protected void doWhileLoop() {
	    xInt = 0;
	    do {
	        xInt++;	        
	    } while(xInt < 10);
	    
	    yInt = 0;           // ERROR Interval "this.y" is not writable because it may not be the current interval.
	    do {
	        yInt++;         // ERROR Interval "this.y" is not writable because it may not be the current interval.
	    } while(yInt < 10); // ERROR Interval "this.y" is not readable because the current interval may not happen after it.
	}
	
	@Requires(subinterval=@Subinterval(of="x"))	
	protected void minimumIterationsAndTheHbRelation(
    	int j, 
    	@Creator("x") Object xObject
    ) {
	    @Creator("hb y") Object hbYObject;

        // Here the loop is not statically known to
        // execute at least one iteration:
        for (int i = j; i < 1; i++) {
	        Intervals.addHb(x, y);            
        }
	    hbYObject = xObject; // ERROR Variable "" has type "java.lang.Object<`ch.ethz.intervals.quals.Creator`: hb this.y>" which is not a subtype of "java.lang.Object<`ch.ethz.intervals.quals.Creator`: hb this.y>".
	    
	    // Also here:
	    int i = j;
	    while(i < 1) {
	        Intervals.addHb(x, y);
	        i++;
	    }
	    hbYObject = xObject; // ERROR Variable "" has type "java.lang.Object<`ch.ethz.intervals.quals.Creator`: hb this.y>" which is not a subtype of "java.lang.Object<`ch.ethz.intervals.quals.Creator`: hb this.y>".
	    
	    // But with do-while it's ok:
	    do {
	        Intervals.addHb(x, y);
	        i++;
	    } while(i < 1);	    
	    hbYObject = xObject; 
	}
	
}
