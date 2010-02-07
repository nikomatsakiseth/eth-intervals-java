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
	        yInt = 0;       // ERROR (intervals.not.writable)
	        yInt < 10;      // ERROR (intervals.not.readable)
	        yInt++          // ERROR (intervals.not.writable)
	    );
	}

	@Requires(subinterval=@Subinterval(of="x"))	
	protected void whileLoop() {
	    xInt = 0;
	    while(xInt < 10) 
	    {
	        xInt++;
	    }
	    
	    yInt = 0;           // ERROR (intervals.not.writable)
	    while(yInt < 10)    // ERROR (intervals.not.readable)
        {
	        yInt++;         // ERROR (intervals.not.writable)
	    }
	}
	
	@Requires(subinterval=@Subinterval(of="x"))	
	protected void doWhileLoop() {
	    xInt = 0;
	    do {
	        xInt++;	        
	    } while(xInt < 10);
	    
	    yInt = 0;           // ERROR (intervals.not.writable)
	    do {
	        yInt++;         // ERROR (intervals.not.writable)
	    } while(yInt < 10); // ERROR (intervals.not.readable)
	}
	
}
