package ch.ethz.intervals.guard;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.Point;
import ch.ethz.intervals.IntervalException.DataRace;
import ch.ethz.intervals.IntervalException.DataRace.Role;

/**
 * A version of the {@link WriteTrackingDynamicGuard} which does not track
 * individual reads, but rather their mutual bound. If more than one read
 * is active, the next writer must <i>happen after</i> their mutual
 * bound as computed by {@link Intervals#mutualBound(Point, Point)}.
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
		public Point bound;

		public SummarizedRead(Point bound) {
			this.bound = bound;
		}
		
		public void add(Point pnt) {
			bound = bound.mutualBound(pnt);
		}
	}

	@Override
	protected Object addActiveReadBoundedBy(
			Object reads,
			Point interEnd) 
	{
		if(reads == null) // no active readers
			return interEnd; // ...now exactly 1
		
		SummarizedRead summary;
		if(reads instanceof SummarizedRead) {
			// already multiple active readers:
			summary = (SummarizedRead) reads;
		} else {
			// used to be just 1 active reader:
			summary = new SummarizedRead((Point) reads);
		}
		
		summary.add(interEnd);
		return summary;
	}

	@Override
	protected void checkHappensAfterActiveReads(
			Point mr,
			Interval inter, 
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
				Point rd = (Point) reads;
				if(rd != inter.getEnd() && !rd.hbeq(mr))
					throw new IntervalException.DataRace(this, interRole, inter, DataRace.READ, rd);
			}
		}
	}

}
