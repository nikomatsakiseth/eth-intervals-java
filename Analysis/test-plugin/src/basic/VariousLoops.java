package basic;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.quals.Creator;
import ch.ethz.intervals.quals.GuardedBy;
import ch.ethz.intervals.quals.Requires;

public abstract class VariousLoops {

    final Interval x;
    final Interval y;
	@GuardedBy("x") int xInt;
	@GuardedBy("y") int yInt;
	
	VariousLoops(Interval x, Interval y) {
	    this.x = x;
	    this.y = y;
	}
	
	abstract boolean cond();
	
	@Requires("method suspends x")	
	protected void oldStyleForLoop(boolean cond1, boolean cond2) {
	    for(
	        xInt = 0;
	        xInt < 10;
	        xInt++
	    ) {
	        if(cond1) continue;
	        if(cond2) break;
	    }
	    
	    for(
	        yInt = 0;       // ERROR Guard "this.(basic.VariousLoops.y)" is not writable.
	        yInt < 10;      // ERROR Guard "this.(basic.VariousLoops.y)" is not readable.
	        yInt++          // ERROR Guard "this.(basic.VariousLoops.y)" is not writable.
	    ) {
	        if(cond1) continue;
	        if(cond2) break;
	    }
	}

	@Requires("method suspends x")	
	protected void whileLoop(boolean cond1, boolean cond2) {
	    xInt = 0;
	    while(xInt < 10) 
	    {
	        xInt++;
	        if(cond1) continue;
	        if(cond2) break;
	    }
	    
	    yInt = 0;           // ERROR Guard "this.(basic.VariousLoops.y)" is not writable.
	    while(yInt < 10)    // ERROR Guard "this.(basic.VariousLoops.y)" is not readable.
        {
	        yInt++;         // ERROR Guard "this.(basic.VariousLoops.y)" is not writable.
	        if(cond1) continue;
	        if(cond2) break;
	    }
	}
	
	@Requires("method suspends x")	
	protected void doWhileLoop(boolean cond1, boolean cond2) {
	    xInt = 0;
	    do {
	        xInt++;	        
	        if(cond1) continue;
	        if(cond2) break;
	    } while(xInt < 10);
	    
	    yInt = 0;           // ERROR Guard "this.(basic.VariousLoops.y)" is not writable.
	    do {
	        yInt++;         // ERROR Guard "this.(basic.VariousLoops.y)" is not writable.
	        if(cond1) continue;
	        if(cond2) break;
	    } while(yInt < 10); // ERROR Guard "this.(basic.VariousLoops.y)" is not readable.
	}
	
	@Requires("method suspends x")	
	protected void forLoopNotGuaranteedToIterate(
    	int j, 
    	@Creator("x") Object xObject
    ) {
	    @Creator("readableBy y") Object readableByYObject;
        for (int i = j; i < 1; i++) {
	        Intervals.addHb(x, y);            
        }
	    readableByYObject = xObject; // ERROR Variable "xObject" has type * which is not a subtype of *.
	}
	    
	@Requires("method suspends x")	
	protected void whileLoopNotGuaranteedToIterate(
    	int j, 
    	@Creator("x") Object xObject
    ) {
	    @Creator("readableBy y") Object readableByYObject;
	    int i = j;
	    while(i < 1) {
	        Intervals.addHb(x, y);
	        i++;
	    }
	    readableByYObject = xObject; // ERROR Variable "xObject" has type * which is not a subtype of *.
    }
    
	@Requires("method suspends x")	
	protected void doWhileLoopWithBreakNotGuaranteedToIterate(
    	int j, 
    	@Creator("x") Object xObject
    ) {
        int i = j;
	    @Creator("readableBy y") Object readableByYObject;
        do {
	        if(cond())
    	        break;
	        Intervals.addHb(x, y);
	        i++;
	    } while(i < 1);	    
	    readableByYObject = xObject; // ERROR Variable "xObject" has type * which is not a subtype of *.
	}

	@Requires("method suspends x")	
	protected void doWhileLoopWithContinueNotGuaranteedToIterate(
    	int j, 
    	@Creator("x") Object xObject
    ) {
        int i = j;
	    @Creator("readableBy y") Object readableByYObject;
	    do {
	        if(cond())
    	        continue;
	        Intervals.addHb(x, y);
	        i++;
	    } while(i < 1);	    
	    readableByYObject = xObject; // ERROR Variable "xObject" has type * which is not a subtype of *.
	}

	@Requires("method suspends x")	
	protected void plainDoWhileLoopGuaranteedToIterate(
    	int j, 
    	@Creator("x") Object xObject
    ) {
        int i = j;
	    @Creator("readableBy y") Object readableByYObject;
	    do {
	        Intervals.addHb(x, y);
	        i++;
	    } while(i < 1);	    
	    readableByYObject = xObject; 
	}
	
}
