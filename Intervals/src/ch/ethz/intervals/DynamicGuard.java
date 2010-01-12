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
public class DynamicGuard 
extends Lock
implements Guard {
	
	private enum State {
		/** Both wr, rd are {@code null} */
		INITIAL,
		
		/** wr is bound of writer, rd is {@code null} */
		WR_OWNED_END,
		
		/** wr is bound of previous writer, rd is bound of singular reader */
		RD_OWNED_END, 
		
		/** wr is bound of previous writer, rd is mutual bound of <b>all readers</b> */
		RD_SHARED 
	}
	
	private State state = State.INITIAL;
	
	/** End of most recent lock owner, or null. */
	private Point lockEnd;
	
	/** End of most recent writer. */
	private Point wr;
	
	/** End of most recent reader, or mutual bound, depending on {@link State}. */
	private Point rd; 
	
	class SavedFields {
		final State state;
		final Point lockEnd, wr, rd;
		final SavedFields stack;
		
		public SavedFields(State state, Point lockEnd, Point wr, Point rd,
				SavedFields stack) {
			this.state = state;
			this.lockEnd = lockEnd;
			this.wr = wr;
			this.rd = rd;
			this.stack = stack;
		}
	}
	
	SavedFields stack = null;
	
	private void pushFields(State nextState, Point nextLockEnd, Point nextWr, Point nextRd) {
		stack = new SavedFields(state, lockEnd, wr, rd, stack);		
		rd = nextRd;
		wr = nextWr;
		lockEnd = nextLockEnd;
		state = nextState;
	}
	
	private void popFields() {
		state = stack.state;
		lockEnd = stack.lockEnd;
		wr = stack.wr;
		rd = stack.rd;
		stack = stack.stack;
	}
	
	@Override
	void didLock(Interval inter) {
		pushFields(State.INITIAL, inter.end, null, null);
	}

	private boolean processLock(Interval inter) {
		if(lockEnd == null)
			return true;
		
		if(inter.end.isBoundedByOrEqualTo(lockEnd))
			return true;
		
		if(lockEnd.hbeq(inter.start)) {
			popFields();
			return processLock(inter);
		}
		
		return false;
	}
	
	@Override
	public boolean isWritable() {		
		Current current = Current.get();
		
		// Root interval:
		//     For now play it safe.  Later think.
		if(current.inter == null)
			return false;
		
		if(!processLock(current.inter))
			return false;
		
		return isWritableBy(current.mr);
	}
	
	private boolean isRepeat(Point bnd, Point mostRecent) {
		return mostRecent.line == bnd.line && mostRecent.isBoundedBy(bnd);
	}

	synchronized boolean isWritableBy(Point mostRecent) {
		switch(state) {
		case INITIAL:
			// Not yet read or written.  Always safe.
			break; 
			
		case WR_OWNED_END:
			// Currently being written by interval starting at "wr".  Safe if either:
			// * wr == curStart (we are already the owner)
			// * wr.nextEpoch hbeq curStart (writer has finished)
			if(isRepeat(wr, mostRecent))
				return true;
			if(!wr.hbeq(mostRecent, EdgeList.SPECULATIVE))
				return false;
			break;
			
		case RD_OWNED_END:
			// Currently being read by interval starting at "rd". Safe if either:
			// * rd is curStart (curStart was the reader, now becomes the writer)
			// * rd.nextEpoch hbeq curStart (reader has finished)
			if(rd != mostRecent.bound && !rd.hbeq(mostRecent, EdgeList.SPECULATIVE))
				return false;
			break;
		
		case RD_SHARED:
			// Currently being read by multiple intervals bounded by "rd".  Safe if:
			// * rd hbeq curStart (all readers have finished)
			if(!rd.hbeq(mostRecent, EdgeList.SPECULATIVE))
				return false;
			break;
		}		
		
		wr = mostRecent.bound;
		rd = null;
		state = State.WR_OWNED_END;
		return true;
	}
	
	@Override
	public boolean isReadable() {
		Current current = Current.get();		
		
		// Root interval:
		//     For now play it safe.  Later think.
		if(current.inter == null)
			return false;
		
		if(!processLock(current.inter))
			return false;
		
		return isReadableBy(current.mr);
	}
	
	synchronized boolean isReadableBy(Point mostRecent) {
		switch(state) {
		case INITIAL: 
			// Not yet read or written.  Always safe, and we are the only writer.
			state = State.RD_OWNED_END;
			rd = mostRecent.bound;
			return true;
		
		case WR_OWNED_END: 
			// Previously written by wr.  Safe if:
			// * wr == curStart (being read by the writer)
			// * wr.nextEpoch hbeq curStart (writer has finished)
			// In the latter case, we become the Rd Owner.
			if(isRepeat(wr, mostRecent))
				return true;
			if(!wr.hbeq(mostRecent, EdgeList.SPECULATIVE))
				return false;
			
			state = State.RD_OWNED_END;
			rd = mostRecent.bound;
			return true;
			
		case RD_OWNED_END:
			// Previously being read only by interval starting at rd.  Safe if:
			// * rd == curStart (rd still rd owner)
			// * (see twoReaders)			
			if(isRepeat(rd, mostRecent))
				return true;
			return twoReaders(mostRecent, rd);
			
		case RD_SHARED:
			return twoReaders(mostRecent, rd);
		}
		assert false; // should never happen
		return false;
	}

	private boolean twoReaders(Point mostRecent, Point curRdEnd) {
		// Being read by one or more intervals, bounded by curRdEnd.  Safe if:
		// * curRdEnd hbeq curStart (all previous readers have finished, curStart becomes sole reader)
		// * wr.nextEpoch bheq curStart (writer has finished)
		// In last case, must adjust rd to include curStart.nextEpoch
		if(curRdEnd.hbeq(mostRecent, EdgeList.SPECULATIVE)) {
			state = State.RD_OWNED_END;
			rd = mostRecent.bound;
			return true;
		}				
		if(wr != null && !wr.hbeq(mostRecent, EdgeList.SPECULATIVE))
			return false;
		state = State.RD_SHARED;
		rd = curRdEnd.mutualBound(mostRecent.bound);
		return true;
	}
		
}
