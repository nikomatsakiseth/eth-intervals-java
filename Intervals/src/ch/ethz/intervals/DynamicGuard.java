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
		Interval curInter = current.inter;
		if(curInter == null)
			return false;		
		return isReadableBy(curInter);
	}

	synchronized boolean isReadableBy(Interval curInter) {
		if(wr == curInter.end) 
			return true; // current interval wrote last, can also read
		if(wr != null && !wr.hb(curInter.start, EdgeList.SPECULATIVE))
			return false; // last write must HB reads
		if(rd == null)
			rd = curInter.end;
		else if(rd != curInter.end)
			rd = rd.mutualBound(curInter.end);
		return true;
	}
	
	@Override
	public boolean isWritable() {
		Current current = Current.get();		
		Interval curInter = current.inter;
		if(curInter == null)
			return false;
		return isWritableBy(curInter);
	}
	
	synchronized boolean isWritableBy(Interval curInter) {
		if(wr == curInter.end) 
			return true; // current interval wrote last
		if(wr != null && !wr.hb(curInter.start, EdgeList.SPECULATIVE))
			return false; // last write must HB next write
		if(rd != null && !rd.hb(curInter.start, EdgeList.SPECULATIVE))
			return false; // last read must HB next write
		rd = null;
		wr = curInter.end;
		return true;
	}
		
}
