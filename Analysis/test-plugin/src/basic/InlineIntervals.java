package basic;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.VoidInlineTask;
import ch.ethz.intervals.quals.Creator;

class InlineIntervalsIncrementData extends VoidInlineTask {
	private final @Creator("writableBy Subinterval") Data data;

	public InlineIntervalsIncrementData(
			@Creator("writableBy Subinterval") Data data
	) {
		this.data = data;
	}

	@Override public void run(Interval subinterval) {
		data.integer++;		
	}
}

public class InlineIntervals {

	public void canWrite(@Creator("writableBy method") Data data) {
		data.integer++;
		Intervals.inline(new InlineIntervalsIncrementData(data));
		data.integer++;
	}
	
	public void cannotWrite(@Creator("readableBy method") Data data) {
		data.integer++; // ERROR foo
		Intervals.inline(new InlineIntervalsIncrementData(data)); // ERROR foo
		data.integer++; // ERROR foo
	}
	
}
