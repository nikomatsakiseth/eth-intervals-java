package bbpc;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Dependency;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.Parent;
import ch.ethz.intervals.ParentForNew;
import ch.ethz.intervals.quals.Creator;
import ch.ethz.intervals.quals.Requires;

public class Producer extends Interval {
	
	final @Creator("readableBy this") ConsData cdata;
	final @Creator("this") ProdData pdata;
	
	public Producer(
	    @ParentForNew("Parent") Dependency p, 
	    final Interval c, 
	    @Creator("c") ConsData cdata
	) {
		super(p);
		Intervals.addHb(c, this);
		this.cdata = cdata;
		this.pdata = new /*@Creator("this")*/ ProdData();
	}

	@Override
	@Requires("method suspends this")
	protected void run() {
		Data data = new /*@Creator("this")*/ Data();
		pdata.data = data;
		
		Producer nextProd = new /*@Parent("parent")*/ Producer(parent, cdata.nextCons, cdata.nextCdata); 
		pdata.nextProd = nextProd;
		pdata.nextPdata = nextProd.pdata;
	}

}
