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
		WR_OWNED,
		
		/** wr is start of previous writer, rd is <b>start</b> of singular reader */
		RD_OWNED, 
		
		/** wr is start of previous writer, rd is mutual bound of <b>ends</b> of all readers */
		RD_SHARED 
	}
	
	private State state = State.INITIAL;	
	private Point wr, rd; /** @see State */
		
	@Override
	public boolean isWritable() {		
		Current current = Current.get();		
		if(current.start == null)
			return false;
		return isWritableBy(current.start);
	}

	synchronized boolean isWritableBy(Point curStart) {
		if(wr == curStart) return true; // shortcircuit repeated writes
		
		switch(state) {
		case INITIAL:
			// Not yet read or written.  Always safe.
			break; 
			
		case WR_OWNED:
			// Currently being written by interval starting at "wr".  Safe if either:
			// * wr == curStart (we are the owner, checked above)
			// * wr.nextEpoch hbeq curStart (writer has finished)
			if(!wr.nextEpoch.hbeq(curStart, EdgeList.SPECULATIVE))
				return false;
			break;
			
		case RD_OWNED:
			// Currently being read by interval starting at "rd". Safe if either:
			// * rd is curStart (curStart was the reader, now becomes the writer)
			// * rd.nextEpoch hbeq curStart (reader has finished)
			if(rd != curStart && !rd.nextEpoch.hbeq(curStart, EdgeList.SPECULATIVE))
				return false;
			break;
		
		case RD_SHARED:
			// Currently being read by multiple intervals bounded by "rd".  Safe if:
			// * rd hbeq curStart (all readers have finished)
			if(!rd.hbeq(curStart, EdgeList.SPECULATIVE))
				return false;
			break;
		}		
		
		wr = curStart;
		rd = null;
		state = State.WR_OWNED;
		return true;
	}
	
	@Override
	public boolean isReadable() {
		Current current = Current.get();		
		if(current.start == null)
			return false;		
		return isReadableBy(current.start);
	}
	
	synchronized boolean isReadableBy(Point curStart) {
		switch(state) {
		case INITIAL: 
			// Not yet read or written.  Always safe, and we are the only writer.
			state = State.RD_OWNED;
			rd = curStart;
			return true;
		
		case WR_OWNED: 
			// Previously written by wr.  Safe if:
			// * wr == curStart (being read by the writer)
			// * wr.nextEpoch hbeq curStart (writer has finished)
			// In the latter case, we become the Rd Owner.
			if(wr == curStart) // Already Wr Owner.  Stay that way.
				return true;
			if(!wr.nextEpoch.hbeq(curStart, EdgeList.SPECULATIVE))
				return false;
			
			state = State.RD_OWNED;
			rd = curStart;
			return true;
			
		case RD_OWNED:
			// Previously being read only by interval starting at rd.  Safe if:
			// * rd == curStart (rd still rd owner)
			// * rd.nextEpoch hbeq curStart (prev rd owner has finished)
			// * wr.nextEpoch hbeq curStart (writer has finished)
			// In the last case, there are now two concurrent readers, so go to RD_SHARED state.
			if(rd == curStart) // Already Rd Owner.  Stay that way.
				return true;
			if(rd.nextEpoch.hbeq(curStart, EdgeList.SPECULATIVE)) {
				rd = curStart;
				return true;
			}	
			if(wr != null && !wr.nextEpoch.hbeq(curStart, EdgeList.SPECULATIVE))
				return false;
			state = State.RD_SHARED;
			rd = rd.nextEpoch.mutualBound(curStart.nextEpoch); // n.b.: mutual bound of END!
			return true;
			
		case RD_SHARED:
			// Prevously being read by multiple intervals, bounded by rd.  Safe if:
			// * rd hbeq curStart (all previous readers have finished, we become sole reader)
			// * wr.nextEpoch bheq curStart (writer has finished)
			// Must adjust rd to include our bound.
			if(rd.hbeq(curStart, EdgeList.SPECULATIVE)) {
				state = State.RD_OWNED;
				rd = curStart;
				return true;
			}				
			if(wr != null && !wr.nextEpoch.hbeq(curStart, EdgeList.SPECULATIVE))
				return false;
			rd = rd.mutualBound(curStart.nextEpoch);
			return true;			
		}
		assert false; // should never happen
		return false;
	}
		
}
