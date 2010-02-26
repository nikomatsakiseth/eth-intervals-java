package erco.intervals.tsp;

/*
 * Copyright (C) 2000 by ETHZ/INF/CS
 * All rights reserved
 * 
 * @version $Id: TourElement.java 2094 2003-01-30 09:41:18Z praun $
 * @author Florian Schneider
 */
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.guard.ReadTrackingDynamicGuard;
import ch.ethz.intervals.quals.GuardedBy;

public class TourElement {
	final ReadTrackingDynamicGuard dg = new ReadTrackingDynamicGuard();
    @GuardedBy("dg") private int[] prefix=new int[Tsp.MAX_TOUR_SIZE];
    @GuardedBy("dg") private int conn;
    @GuardedBy("dg") private int last;
    @GuardedBy("dg") private int prefix_weight;
    @GuardedBy("dg") private int lower_bound;
    @GuardedBy("dg") private int mst_weight;
    
	void setConn(int conn) {		
		assert Intervals.checkWritable(dg);
		this.conn = conn;
	}
	int conn() {
		assert Intervals.checkReadable(dg);
		return conn;
	}
	int setLast(int last) {
		assert Intervals.checkWritable(dg);
		this.last = last;
		return last;
	}
	int last() {
		assert Intervals.checkReadable(dg);
		return last;
	}
	void setPrefix_weight(int prefix_weight) {
		assert Intervals.checkWritable(dg);
		this.prefix_weight = prefix_weight;
	}
	int prefix_weight() {
		assert Intervals.checkReadable(dg);
		return prefix_weight;
	}
	void setLower_bound(int lower_bound) {
		assert Intervals.checkWritable(dg);
		this.lower_bound = lower_bound;
	}
	int lower_bound() {
		assert Intervals.checkReadable(dg);
		return lower_bound;
	}
	void setMst_weight(int mst_weight) {
		assert Intervals.checkWritable(dg);
		this.mst_weight = mst_weight;
	}
	int mst_weight() {
		assert Intervals.checkReadable(dg);
		return mst_weight;
	}
	void setPrefix(int[] prefix) {
		assert Intervals.checkWritable(dg);
		this.prefix = prefix;
	}
	int[] prefix() {
		assert Intervals.checkReadable(dg);
		return prefix;
	}
}
