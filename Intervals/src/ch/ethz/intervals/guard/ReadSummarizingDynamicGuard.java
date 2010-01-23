package ch.ethz.intervals.guard;

import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.IntervalException.DataRace;
import ch.ethz.intervals.IntervalException.DataRace.Role;
import ch.ethz.intervals.mirror.IntervalMirror;
import ch.ethz.intervals.mirror.PointMirror;

/**
 * A version of the {@link WriteTrackingDynamicGuard} which does not track
 * individual reads, but rather their mutual bound. If more than one read
 * is active, the next writer must <i>happen after</i> their mutual
 * bound as computed by {@link Intervals#mutualBound(PointMirror, PointMirror)}.
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
		public PointMirror bound;

		public SummarizedRead(PointMirror bound) {
			this.bound = bound;
		}
		
		public void add(PointMirror pnt) {
			bound = Intervals.mutualBound(bound, pnt);
		}
	}

	@Override
	protected Object addActiveReadBoundedBy(
			Object reads,
			PointMirror interEnd) 
	{
		if(reads == null) // no active readers
			return interEnd; // ...now exactly 1
		
		SummarizedRead summary;
		if(reads instanceof SummarizedRead) {
			// already multiple active readers:
			summary = (SummarizedRead) reads;
		} else {
			// used to be just 1 active reader:
			summary = new SummarizedRead((PointMirror) reads);
		}
		
		summary.add(interEnd);
		return summary;
	}

	@Override
	protected void checkHappensAfterActiveReads(
			PointMirror mr,
			IntervalMirror inter, 
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
				PointMirror rd = (PointMirror) reads;
				if(rd != inter.end() && !rd.hbeq(mr))
					throw new IntervalException.DataRace(this, interRole, inter, DataRace.READ, rd);
			}
		}
	}

}
