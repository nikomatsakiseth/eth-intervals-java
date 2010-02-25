package basic;

import java.util.List;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.guard.Guard;
import ch.ethz.intervals.quals.Creator;
import ch.ethz.intervals.quals.DefinesGhost;
import ch.ethz.intervals.quals.GuardedBy;
import ch.ethz.intervals.quals.Requires;

class Lists {

    final Guard x;
    final List</*@Creator("x")*/ Data> lst;
    
    Lists(Guard x, List</*@Creator("x")*/ Data> lst) {
        this.x = x;
        this.lst = lst;
    }
    
	@Requires("x writableBy method")	
	protected void writeMemberWithWritableBy() {
	    Data data = lst.get(0);
	    data.integer = 22;
	}
    
	@Requires("x readableBy method")	
	protected void writeMemberWithReadableBy() {
	    Data data = lst.get(0);
	    data.integer = 22; // ERROR Guard "this.(basic.Lists.x)" is not writable.
	}
    
	@Requires("x writableBy method")	
	protected int readMemberWithWritableBy() {
	    Data data = lst.get(0);
	    return data.integer;
	}
    
	@Requires("x readableBy method")	
	protected int readMemberWithReadableBy() {
	    Data data = lst.get(0);
	    return data.integer;
	}
    
}