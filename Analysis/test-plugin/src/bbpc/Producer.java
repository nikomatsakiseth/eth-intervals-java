package bbpc;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.quals.Creator;
import ch.ethz.intervals.quals.Requires;
import ch.ethz.intervals.quals.Subinterval;

public class Producer extends Interval {
	
	final @Creator("hb this") ConsData cdata;
	final @Creator("this") ProdData pdata;
	
	public Producer(Interval c, @Creator("c") ConsData cdata) {
		super(c);
		Intervals.addHb(c, this);
		this.cdata = cdata;
		this.pdata = new /*@Creator("this")*/ ProdData();
	}

	@Override
	@Requires(subinterval=@Subinterval(of="this"))	
	protected void run() {
		Data data = new /*@Creator("this")*/ Data();
		pdata.data = data;
		
		Producer nextProd = new Producer(cdata.nextCons, cdata.nextCdata); 
		pdata.nextProd = nextProd;
		pdata.nextPdata = nextProd.pdata;
	}

}
