package basic;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.VoidInlineTask;
import ch.ethz.intervals.quals.Creator;
import ch.ethz.intervals.quals.Requires;

class InlineIntervalsIncrementData extends VoidInlineTask {
	private final @Creator("writableBy Subinterval") Data data;

	public InlineIntervalsIncrementData(
			@Creator("writableBy Subinterval") Data data
	) {
		this.data = data;
	}

	@Requires("method suspends Subinterval")
	@Override public void run(Interval subinterval) {
		data.integer++;		
	}
}

public class InlineIntervals {

	public void canWrite(@Creator("writableBy method") Data data) {
		data.integer = 22;
		Intervals.inline(new InlineIntervalsIncrementData(data));
	}

	public void cannotWrite(@Creator("readableBy method") Data data) {
		// ^ERROR Variable * was not declared.
		data.integer = 22; // ERROR Guard "data.(ch.ethz.intervals.quals.Creator)" is not writable.
		Intervals.inline(new InlineIntervalsIncrementData(data)); // ERROR Variable "data" has type * which is not a subtype of *.
	}
	
}
