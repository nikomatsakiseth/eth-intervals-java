package basic;

import java.util.List;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.guard.Guard;
import ch.ethz.intervals.quals.Creator;
import ch.ethz.intervals.quals.DefinesGhost;
import ch.ethz.intervals.quals.GuardedBy;
import ch.ethz.intervals.quals.Requires;

class If {

    void test(Interval x, @Creator("readableBy x") Data data) {
        
    }
    
	protected void noAdd(Interval x, Interval y, @Creator("x") Data data) {
	    test(y, data); // ERROR Variable "data" has type * which is not a subtype of *.
	}
    
	protected void uncondAdd(Interval x, Interval y, @Creator("x") Data data) {
	    Intervals.addHb(x, y);
	    test(y, data);
	}
    
	protected void condAdd(boolean cond, Interval x, Interval y, @Creator("x") Data data) {
	    if(cond)
    	    Intervals.addHb(x, y);	        
	    test(y, data); // ERROR Variable "data" has type * which is not a subtype of *.
	}

	protected void bothAdd(boolean cond, Interval x, Interval y, @Creator("x") Data data) {
	    if(cond)
    	    Intervals.addHb(x, y);
    	else        
    	    Intervals.addHb(x, y);
	    test(y, data);
	}

    
}