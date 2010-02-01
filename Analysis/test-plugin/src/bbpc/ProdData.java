package bbpc;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.quals.Creator;

public class ProdData {
	public Data data;
	
	public Interval nextProd;
	public @Creator("nextProd") ProdData nextPdata;
}
