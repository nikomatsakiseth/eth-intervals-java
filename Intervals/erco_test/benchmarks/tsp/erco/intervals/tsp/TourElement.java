package erco.intervals.tsp;

import ch.ethz.intervals.DynamicGuard;
/*
 * Copyright (C) 2000 by ETHZ/INF/CS
 * All rights reserved
 * 
 * @version $Id: TourElement.java 2094 2003-01-30 09:41:18Z praun $
 * @author Florian Schneider
 */
import ch.ethz.intervals.quals.GuardedBy;

public class TourElement {
	final DynamicGuard dg = new DynamicGuard();
    @GuardedBy("dg") private int[] prefix=new int[Tsp.MAX_TOUR_SIZE];
    @GuardedBy("dg") private int conn;
    @GuardedBy("dg") private int last;
    @GuardedBy("dg") private int prefix_weight;
    @GuardedBy("dg") private int lower_bound;
    @GuardedBy("dg") private int mst_weight;
    
	void setConn(int conn) {
		//assert dg.isWritable();
		this.conn = conn;
	}
	int conn() {
		//assert dg.isReadable();
		return conn;
	}
	int setLast(int last) {
		//assert dg.isWritable();
		this.last = last;
		return last;
	}
	int last() {
		//assert dg.isReadable();
		return last;
	}
	void setPrefix_weight(int prefix_weight) {
		//assert dg.isWritable();
		this.prefix_weight = prefix_weight;
	}
	int prefix_weight() {
		//assert dg.isReadable();
		return prefix_weight;
	}
	void setLower_bound(int lower_bound) {
		//assert dg.isWritable();
		this.lower_bound = lower_bound;
	}
	int lower_bound() {
		//assert dg.isReadable();
		return lower_bound;
	}
	void setMst_weight(int mst_weight) {
		//assert dg.isWritable();
		this.mst_weight = mst_weight;
	}
	int mst_weight() {
		//assert dg.isReadable();
		return mst_weight;
	}
	void setPrefix(int[] prefix) {
		//assert dg.isWritable();
		this.prefix = prefix;
	}
	int[] prefix() {
		//assert dg.isReadable();
		return prefix;
	}
}
