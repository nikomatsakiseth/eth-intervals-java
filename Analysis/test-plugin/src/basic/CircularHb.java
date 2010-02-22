package basic;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.quals.Requires;
import ch.ethz.intervals.quals.Subinterval;
import ch.ethz.intervals.quals.Happens;
import ch.ethz.intervals.quals.DefinesGhost;
import ch.ethz.intervals.quals.Creator;
import ch.ethz.intervals.quals.GuardedBy;

class CircularHbData {
    int i;
}

class CircularHb {

	@Requires(subinterval=@Subinterval(of="y"))
    void method(
        Interval x, 
        Interval y,
        @Creator("x") CircularHbData xData,
        @Creator("y") CircularHbData yData
    ) {
        Intervals.addHb(x, y);
        Intervals.addHb(y, x);
        
        int xi = xData.i;
        int yi = yData.i;
        xData.i = yi; // ERROR Interval "x" is not writable because it may not be the current interval.
        yData.i = xi;
    }
    
}