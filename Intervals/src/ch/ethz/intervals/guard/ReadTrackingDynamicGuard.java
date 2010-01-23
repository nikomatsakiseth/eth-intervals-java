package ch.ethz.intervals.guard;

import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.IntervalException.DataRace;
import ch.ethz.intervals.IntervalException.DataRace.Role;
import ch.ethz.intervals.mirror.IntervalMirror;
import ch.ethz.intervals.mirror.PointMirror;
import ch.ethz.intervals.util.ChunkList;

/**
 * A dynamic guard which tracks every active reader.  This provides
 * maximum precision and avoids false errors, but also takes more memory
 * if the data it is guarded enters into a "read-only" state where it will
 * not be written again.
 */
public class ReadTrackingDynamicGuard
extends WriteTrackingDynamicGuard<ChunkList<PointMirror>> {

	public ReadTrackingDynamicGuard() {
		super();
	}

	public ReadTrackingDynamicGuard(String name) {
		super(name);
	}

	@Override
	protected void checkHappensAfterActiveReads(
			final PointMirror mr, 
			final IntervalMirror inter, 
			final Role interRole,
			final ChunkList<PointMirror> reads)
	{
		final PointMirror interEnd = inter.end();
		if(reads != null) {
			new ChunkList.Iterator<PointMirror>(reads) {
				@Override public void doForEach(PointMirror rd, int _) {
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
	protected ChunkList<PointMirror> addActiveReadBoundedBy(
			ChunkList<PointMirror> reads, 
			PointMirror interEnd) 
	{
		return ChunkList.add(reads, interEnd, ChunkList.NO_FLAGS);
	}
}