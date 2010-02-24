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
	    yString = "y"; // ERROR Interval "this.(basic.WriteFromWrongInterval.y)" is not writable because it may not be the current interval.
	}

	@Requires("method suspends y")	
	protected void duringY() {
	    xString = "x"; // ERROR Interval "this.(basic.WriteFromWrongInterval.x)" is not writable because it may not be the current interval.
	    yString = "y";
	}

}
