package basic;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.quals.Requires;
import ch.ethz.intervals.quals.Subinterval;

public class ParseReqs {
	
	Interval inter;

	@Requires(subinterval=@Subinterval(of="inter"))
	void subinter() {}
	
}
