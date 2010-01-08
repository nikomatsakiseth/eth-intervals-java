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
	
	/* Dynamic guards have four states:
	 * <ul>
	 * 
	 * <li><b>Initial:</b> The data has never been read or written.
	 * 
	 * <li><b>Wr Owned:</b> The data is being written by the interval {@code wr}.
	 *     <ul>
	 *     <li>Any other interval to read or write must <i>happen after</i> {@code wr}.
	 *     <li>The state becomes Rd Owned or Wr Owned accordingly.
	 *     </ul>
	 *  
	 * <li><b>Rd Owned:</b> The data is being read by the interval ending at {@code rd}
	 * and was written by the interval ending at {@code wr}.
	 *     <ul>
	 *     <li>If {@code rd} tries to write, the state is converted to Wr Owned by {@code rd}.
	 *     <li>Any other interval trying to read must <i>happen after</i> {@code wr}.
	 *     The state becomes Rd Shared.
	 *     <li>Any other interval trying to write must <i>happen after</i> {@code rd}.
	 *     The state becomes Wr Owned.
	 *     </ul>
	 * 
	 * <li><b>Rd Shared:</b> The data has been read but only by one interval.
	 * The data is being read by multiple intervals bounded by {@code rd}
	 * and was last written by {@code wr}.
	 *     <ul>
	 *     <li>Any interval trying to read must <i>happen after</i> {@code wr}.
	 *     The end of that interval is incorporated into the bound.
	 *     <li>Any interval trying to write must <i>happen after</i> {@code rd}.
	 *     The state becomes Wr Owned.
	 *     </ul>
	 * 
	 * </ul>
	 */
	
	/** End of last interval to write data guarded by this guard. */
	private Point wr;
	
	/** End of last interval to read data guarded by this guard. */
	private Point rd;
	
	/** End of single reader */
	private boolean rdOwned;
		
	@Override
	public boolean isWritable() {		
		Current current = Current.get();		
		if(current.start == null)
			return false;		
		return isWritableBy(current.start, current.end);
	}

	synchronized boolean isWritableBy(Point curStart, Point curEnd) { 
		if(wr == curEnd) {
			// Current interval was last writer.
			return true; 
		} else if(rd != null) {
			// If single previous reader, must either be us or we must come after.
			// If previous readers, we must come after them.
			//    Note: this implies that wr.hbeq(curStart)
			if(!(rdOwned && rd == curEnd) && !rd.hbeq(curStart, EdgeList.SPECULATIVE))
				return false;
		} else if (wr != null) {
			// If no previous reader, but previous write, we must come after.
			if(!wr.hbeq(curStart, EdgeList.SPECULATIVE))
				return false;
		}
				
		rdOwned = false;
		rd = null;
		wr = curEnd;
		return true;
	}
	
	@Override
	public boolean isReadable() {
		Current current = Current.get();		
		if(current.start == null)
			return false;		
		return isReadableBy(current.start, current.end);
	}
	
	synchronized boolean isReadableBy(Point curStart, Point curEnd) {
		if(wr == curEnd || rd == curEnd)
			return true; // current interval read or wrote last
		
		if(rd != null && rd.hbeq(curStart, EdgeList.SPECULATIVE)) {
			// This read does not overlap with any previous reader:
			//    Note: this implies that wr.hbeq(curStart)
			rdOwned = true; // Exclusive ownership.
			rd = curEnd;
			return true;
		}
		
		if(wr != null && !wr.hbeq(curStart, EdgeList.SPECULATIVE)) {
			// This read does not come after the write:
			return false;
		}
		
		if(rd != null) {
			// This read may overlap with previous readers:
			rdOwned = false; // Shared ownership.
			rd = rd.mutualBound(curEnd);
			return true;
		} 
		
		// No previous reader and we come after the write:
		rdOwned = true; // Exclusive ownership.
		rd = curEnd;
		return true;
	}
		
}
