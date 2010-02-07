package basic;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.quals.Creator;
import ch.ethz.intervals.quals.Requires;
import ch.ethz.intervals.quals.Subinterval;
import ch.ethz.intervals.quals.GuardedBy;

public class WriteFromWrongInterval {

    final Interval x;
    final Interval y;
	@GuardedBy("x") String xString;
	@GuardedBy("y") String yString;
	
	WriteFromWrongInterval(Interval x, Interval y) {
	    this.x = x;
	    this.y = y;
	}
	
	@Requires(subinterval=@Subinterval(of="x"))	
	protected void duringX() {
	    xString = "x";
	    yString = "y"; // ERROR Interval "this.y" is not writable because it may not be the current interval.
	}

	@Requires(subinterval=@Subinterval(of="y"))	
	protected void duringY() {
	    xString = "x"; // ERROR Interval "this.x" is not writable because it may not be the current interval.
	    yString = "y";
	}

}
