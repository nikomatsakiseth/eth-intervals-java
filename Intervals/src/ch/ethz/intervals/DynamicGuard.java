package ch.ethz.intervals;

/**
 * Dynamic guards monitor field accesses dynamically to guarantee 
 * that no race conditions occur.  They require that all writes
 * to data sharing the same dynamic guard <em>happen before</em>
 * all other writes as well as any reads.  
 * 
 * <p>Reads do not have to happen before
 * one another, but they must <em>happen before</em> any subsequent reads.
 * If two intervals {@code i1} and {@code i2} read in parallel, then the next write must
 * <em>happen after</em> the mutual bound of {@code i1.end}, {@code i2.end}, as defined
 * by {@link Point#mutualBound(Point)}.
 */
public class DynamicGuard implements Guard {
	
	/** End of last interval to write data guarded by this guard. */
	private Point wr;
	
	/** Mutual bound of ends of all interval to read data guarded by this guard. */
	private Point rd;
	
	@Override
	public boolean isReadable() {		
		Current current = Current.get();		
		if(current.start == null)
			return false;		
		return isReadableBy(current.start, current.end);
	}

	synchronized boolean isReadableBy(Point curStart, Point curEnd) { 
		if(wr == curEnd) 
			return true; // current interval wrote last, can also read
		if(wr != null && !wr.hb(curStart, EdgeList.SPECULATIVE))
			return false; // last write must HB reads
		if(rd == null)
			rd = curEnd;
		else if(rd != curEnd)
			rd = rd.mutualBound(curEnd);
		return true;
	}
	
	@Override
	public boolean isWritable() {
		Current current = Current.get();		
		if(current.start == null)
			return false;		
		return isWritableBy(current.start, current.end);
	}
	
	synchronized boolean isWritableBy(Point curStart, Point curEnd) {
		if(wr == curEnd) 
			return true; // current interval wrote last
		if(wr != null && !wr.hb(curStart, EdgeList.SPECULATIVE))
			return false; // last write must HB next write
		if(rd != null && !rd.hb(curStart, EdgeList.SPECULATIVE))
			return false; // last read must HB next write
		rd = null;
		wr = curEnd;
		return true;
	}
		
}
