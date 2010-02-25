package basic;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.quals.GuardedBy;
import ch.ethz.intervals.quals.Requires;

public class WriteFromWrongInterval {

    final Interval x;
    final Interval y;
	@GuardedBy("x") String xString;
	@GuardedBy("y") String yString;
	
	WriteFromWrongInterval(Interval x, Interval y) {
	    this.x = x;
	    this.y = y;
	}
	
	@Requires("method suspends x")	
	protected void duringX() {
	    xString = "x";
	    yString = "y"; // ERROR Guard "this.(basic.WriteFromWrongInterval.y)" is not writable.
	}

	@Requires("method suspends y")	
	protected void duringY() {
	    xString = "x"; // ERROR Guard "this.(basic.WriteFromWrongInterval.x)" is not writable.
	    yString = "y";
	}

}
