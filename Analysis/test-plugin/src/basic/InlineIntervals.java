package basic;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.VoidInlineTask;
import ch.ethz.intervals.quals.Creator;

public class InlineIntervals {

	public void canWrite(final @Creator("writableBy method") Data data) {
		data.integer++;
		Intervals.inline(new VoidInlineTask() {			
			@Override public void run(Interval subinterval) {
				data.integer++;				
			}
		});
		data.integer++;
	}
	
	public void cannotWrite(final @Creator("readableBy method") Data data) {
		data.integer++; // ERROR foo
		Intervals.inline(new VoidInlineTask() {			
			@Override public void run(Interval subinterval) {
				data.integer++; // ERROR foo		
			}
		});
		data.integer++; // ERROR foo
	}
	
}
