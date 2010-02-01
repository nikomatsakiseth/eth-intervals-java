package bbpc;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.quals.Creator;

public class ConsData {
	public Interval nextCons;
	public @Creator("nextCons") ConsData nextCdata;
}
