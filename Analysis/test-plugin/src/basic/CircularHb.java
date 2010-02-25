package basic;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.quals.Creator;
import ch.ethz.intervals.quals.Requires;

class CircularHbData {
    int i;
}

class CircularHb {

	@Requires("method suspends y")
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
        xData.i = yi; // ERROR Guard "x" is not writable.
        yData.i = xi;
    }
    
}