package ch.ethz.intervals;

/**
 * Dynamic guards monitor field accesses dynamically to guarantee 
 * that no race conditions occur.  They require that all writes
 * to data sharing the same dynamic guard <em>happen before</em>
 * all other writes as well as any reads.  
 * 
 * <p>Whenever there are multiple reads which are not ordered by
 * <i>happens before</i> relations, the next write must occur
 * after the mutual bound of all reads, as defined by
 * {@link Point#mutualBound(Point)}.
 */
public class DynamicGuard implements Guard {
	
	private enum State {
		/** Both wr, rd are {@code null} */
		INITIAL,
		
		/** wr is start of writer, rd is {@code null} */
		WR_OWNED_START,
		
		/** wr is start of previous writer, rd is <b>start</b> of singular reader */
		RD_OWNED_START, 
		
		/** wr is start of previous writer, rd is mutual bound of <b>ends</b> of all readers */
		RD_SHARED 
	}
	
	private State state = State.INITIAL;	
	private Point wr, rd; /** @see State */
		
	@Override
	public boolean isWritable() {		
		Current current = Current.get();		
		if(current.mr == null)
			return false;
		return isWritableBy(current.mr);
	}

	synchronized boolean isWritableBy(Point mostRecent) {
		if(wr == mostRecent) return true; // shortcircuit repeated writes
		
		switch(state) {
		case INITIAL:
			// Not yet read or written.  Always safe.
			break; 
			
		case WR_OWNED_START:
			// Currently being written by interval starting at "wr".  Safe if either:
			// * wr == curStart (we are the owner, checked above)
			// * wr.nextEpoch hbeq curStart (writer has finished)
			if(!wr.nextEpoch.hbeq(mostRecent, EdgeList.SPECULATIVE))
				return false;
			break;
			
		case RD_OWNED_START:
			// Currently being read by interval starting at "rd". Safe if either:
			// * rd is curStart (curStart was the reader, now becomes the writer)
			// * rd.nextEpoch hbeq curStart (reader has finished)
			if(rd != mostRecent && !rd.nextEpoch.hbeq(mostRecent, EdgeList.SPECULATIVE))
				return false;
			break;
		
		case RD_SHARED:
			// Currently being read by multiple intervals bounded by "rd".  Safe if:
			// * rd hbeq curStart (all readers have finished)
			if(!rd.hbeq(mostRecent, EdgeList.SPECULATIVE))
				return false;
			break;
		}		
		
		wr = mostRecent;
		rd = null;
		state = State.WR_OWNED_START;
		return true;
	}
	
	@Override
	public boolean isReadable() {
		Current current = Current.get();		
		if(current.mr == null)
			return false;		
		return isReadableBy(current.mr);
	}
	
	synchronized boolean isReadableBy(Point mostRecent) {
		switch(state) {
		case INITIAL: 
			// Not yet read or written.  Always safe, and we are the only writer.
			state = State.RD_OWNED_START;
			rd = mostRecent;
			return true;
		
		case WR_OWNED_START: 
			// Previously written by wr.  Safe if:
			// * wr == curStart (being read by the writer)
			// * wr.nextEpoch hbeq curStart (writer has finished)
			// In the latter case, we become the Rd Owner.
			if(wr == mostRecent) // Already Wr Owner.  Stay that way.
				return true;
			if(!wr.nextEpoch.hbeq(mostRecent, EdgeList.SPECULATIVE))
				return false;
			
			state = State.RD_OWNED_START;
			rd = mostRecent;
			return true;
			
		case RD_OWNED_START:
			// Previously being read only by interval starting at rd.  Safe if:
			// * rd == curStart (rd still rd owner)
			// * (see twoReaders)			
			if(rd == mostRecent) // Already Rd Owner.  Stay that way.
				return true;			
			return twoReaders(mostRecent, rd.nextEpoch);
			
		case RD_SHARED:
			return twoReaders(mostRecent, rd);
		}
		assert false; // should never happen
		return false;
	}

	private boolean twoReaders(Point curStart, Point curRdEnd) {
		// Being read by one or more intervals, bounded by curRdEnd.  Safe if:
		// * curRdEnd hbeq curStart (all previous readers have finished, curStart becomes sole reader)
		// * wr.nextEpoch bheq curStart (writer has finished)
		// In last case, must adjust rd to include curStart.nextEpoch
		if(curRdEnd.hbeq(curStart, EdgeList.SPECULATIVE)) {
			state = State.RD_OWNED_START;
			rd = curStart;
			return true;
		}				
		if(wr != null && !wr.nextEpoch.hbeq(curStart, EdgeList.SPECULATIVE))
			return false;
		state = State.RD_SHARED;
		rd = curRdEnd.mutualBound(curStart.nextEpoch);
		return true;
	}
		
}
