package basic;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.quals.Creator;
import ch.ethz.intervals.quals.Requires;
import ch.ethz.intervals.quals.Subinterval;

public class WriteFromWrongInterval {

    final Interval x;
    final Interval y;
	@GuardedBy("x") String xString;
	@GuardedBy("y") String yString;
	
	@Requires(subinterval=@Subinterval(of="x"))	
	protected void duringX() {
	    xString = "x";
	    yString = "y";
	}

	@Requires(subinterval=@Subinterval(of="y"))	
	protected void duringY() {
	    xString = "x";
	    yString = "y";
	}

}
