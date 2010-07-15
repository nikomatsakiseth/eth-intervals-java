package ch.ethz.intervals.guard;

import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.IntervalException.DataRace;
import ch.ethz.intervals.IntervalException.DataRace.Role;
import ch.ethz.intervals.RoInterval;
import ch.ethz.intervals.RoPoint;

/**
 * A version of the {@link WriteTrackingDynamicGuard} which does not track
 * individual reads, but rather their mutual bound. If more than one read
 * is active, the next writer must <i>happen after</i> their mutual
 * bound as computed by {@link RoPoint#mutualBound(RoPoint)}.
 * This checker is sufficient for most usages.
 */
public class ReadSummarizingDynamicGuard extends WriteTrackingDynamicGuard<Object> {
	
	public ReadSummarizingDynamicGuard() {
		super();
	}

	public ReadSummarizingDynamicGuard(String name) {
		super(name);
	}

	private static class SummarizedRead {
		public RoPoint bound;

		public SummarizedRead(RoPoint bound) {
			this.bound = bound;
		}
		
		public void add(RoPoint pnt) {
			bound = bound.mutualBound(pnt);
		}
	}

	@Override
	protected Object addActiveReadBoundedBy(
			Object reads,
			RoPoint interEnd) 
	{
		if(reads == null) // no active readers
			return interEnd; // ...now exactly 1
		
		SummarizedRead summary;
		if(reads instanceof SummarizedRead) {
			// already multiple active readers:
			summary = (SummarizedRead) reads;
		} else {
			// used to be just 1 active reader:
			summary = new SummarizedRead((RoPoint) reads);
		}
		
		summary.add(interEnd);
		return summary;
	}

	@Override
	protected void checkHappensAfterActiveReads(
			RoPoint mr,
			RoInterval inter, 
			Role interRole, 
			Object reads) 
	{
		if(reads != null) {
			if(reads instanceof SummarizedRead) {
				// Multiple reads active:
				//    Note: if null, then the bound is effectively root.end
				SummarizedRead summary = (SummarizedRead) reads;
				if(summary.bound == null || !summary.bound.hbeq(mr))
					throw new IntervalException.DataRace(this, interRole, inter, DataRace.READ, summary.bound);
			} else {
				// Precisely one read:
				//    Note: if null, then the bound is effectively root.end
				RoPoint rd = (RoPoint) reads;
				if(rd != inter.getEnd() && !rd.hbeq(mr))
					throw new IntervalException.DataRace(this, interRole, inter, DataRace.READ, rd);
			}
		}
	}

}
