package ch.ethz.intervals.guard;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.IntervalException.DataRace;
import ch.ethz.intervals.IntervalException.DataRace.Role;
import ch.ethz.intervals.Point;
import ch.ethz.intervals.util.ChunkList;

/**
 * A dynamic guard which tracks every active reader.  This provides
 * maximum precision and avoids false errors, but also takes more memory
 * if the data it is guarded enters into a "read-only" state where it will
 * not be written again.
 */
public class ReadTrackingDynamicGuard
extends WriteTrackingDynamicGuard<ChunkList<Point>> {

	public ReadTrackingDynamicGuard() {
		super();
	}

	public ReadTrackingDynamicGuard(String name) {
		super(name);
	}

	@Override
	protected void checkHappensAfterActiveReads(
			final Point mr, 
			final Interval inter, 
			final Role interRole,
			final ChunkList<Point> reads)
	{
		final Point interEnd = inter.getEnd();
		if(reads != null) {
			new ChunkList.Iterator<Point>(reads) {
				@Override public void doForEach(Point rd, int _) {
					if(rd != interEnd && !rd.hbeq(mr))
						throw new IntervalException.DataRace(
								ReadTrackingDynamicGuard.this, 
								interRole, inter,
								DataRace.READ, rd);
				}
			};
		}
	}

	@Override
	protected ChunkList<Point> addActiveReadBoundedBy(
			ChunkList<Point> reads, 
			Point interEnd) 
	{
		return ChunkList.add(reads, interEnd, ChunkList.NO_FLAGS);
	}
}