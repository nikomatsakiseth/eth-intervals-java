package ch.ethz.intervals;

import ch.ethz.intervals.IntervalException.DataRace;
import ch.ethz.intervals.IntervalException.DataRace.Role;
import ch.ethz.intervals.guard.DynamicGuard;
import ch.ethz.intervals.mirror.IntervalMirror;
import ch.ethz.intervals.mirror.LockMirror;
import ch.ethz.intervals.mirror.PointMirror;


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
public class DefaultDynamicGuard 
implements DynamicGuard {
	
	private final String name;
	
	private final static class Owner {
		final PointMirror end;
		final LockMirror lock;
		final Owner prev;
		
		private Owner(PointMirror bound, LockMirror lock, Owner prev) {
			this.end = bound;
			this.lock = lock;
			this.prev = prev;
		}
		
		public boolean bounds(IntervalMirror inter) {
			return (end == null) || inter.end().isBoundedBy(end);
		}
	}
	
	private static final Owner rootOwner = new Owner(null, null, null); 
	
	private final static class State {
		Owner owner;
		PointMirror mrw;
		LockMirror mrl;
		ChunkList<PointMirror> activeReads;
	}
	
	/** current owner */
	Owner owner = rootOwner;
	
	/** end of most recent write within current owner */
	PointMirror mrw = null;
	
	/** end of potentially active reads within current owner */
	ChunkList<PointMirror> activeReads = null;
	
	public DefaultDynamicGuard() {
		this(null);
	}
	
	public DefaultDynamicGuard(String name) {
		this.name = name;
	}
	
	@Override public String toString() {
		if(name != null)
			return name;
		return super.toString();
	}
	
	/**
	 * Given that 'mr' has occurred, finds and returns the new state:
	 * <ul>
	 * <li> Pops any owners which must have terminated.
	 * <li> Sets most recent write to end of the outermost interval to be popped,
	 *      or {@link #mrw} if none are popped.
	 * <li> Drops any active reads which were bounded by a popped owner. 
	 * </ul>  
	 */
	private State walkBack(PointMirror mr, IntervalMirror inter) {
		State result = new State();
		if(owner.bounds(inter)) {
			result.owner = owner;
			result.mrw = mrw;
			result.mrl = null;
			result.activeReads = activeReads;			
		} else {
			Owner o = owner;
			do {
				result.mrw = o.end;
				result.mrl = o.lock;
				o = o.prev;
			} while(!o.bounds(inter));			
			result.activeReads = null;
			result.owner = o;
		}
		return result;
	}
	
	private Role mrRole(State result) {
		return (result.mrl != null ? new DataRace.LockRole(result.mrl) : DataRace.WRITE);
	}
	
	private void checkHappensAfterMostRecentWrite(
			final PointMirror mr,
			final IntervalMirror inter, 
			final Role interRole,
			State result) 
	{
		if(result.mrw != null && !result.mrw.hbeq(mr))
			throw new IntervalException.DataRace(this, interRole, inter, mrRole(result), result.mrw);
	}

	private void checkHappensAfterActiveReads(
			final PointMirror mr,
			final IntervalMirror inter, 
			final Role interRole,
			State result) 
	{
		final PointMirror interEnd = inter.end();
		if(result.activeReads != null) {
			new ChunkList.Iterator<PointMirror>(result.activeReads) {
				@Override public void doForEach(PointMirror rd, int _) {
					if(rd != interEnd && !rd.hbeq(mr))
						throw new IntervalException.DataRace(
								DefaultDynamicGuard.this, 
								interRole, inter,
								DataRace.READ, rd);
				}
			};
		}
	}
	
	@Override
	public synchronized IntervalException checkReadable(PointMirror mr, IntervalMirror inter) {
		PointMirror interEnd = inter.end();
		
		// Read by owner, ok.
		if(owner.end == interEnd)
			return null;
		
		State result = walkBack(mr, inter);		
		try {
			checkHappensAfterMostRecentWrite(mr, inter, DataRace.READ, result);
		} catch (IntervalException.DataRace err) {
			return err;
		}
		
		// read is permitted:
		owner = result.owner;
		mrw = result.mrw;
		activeReads = ChunkList.add(result.activeReads, interEnd, ChunkList.NO_FLAGS);
		return null;
	}
	

	@Override
	public synchronized IntervalException checkWritable(final PointMirror mr, final IntervalMirror inter) {
		PointMirror interEnd = inter.end();
		
		// Write by owner, ok.
		if(owner.end == interEnd)
			return null;
		
		State result = walkBack(mr, inter);
		
		try {
			checkHappensAfterMostRecentWrite(mr, inter, DataRace.WRITE, result);
			checkHappensAfterActiveReads(mr, inter, DataRace.WRITE, result);
		} catch (IntervalException.DataRace err) {
			return err;
		}
		
		// write is permitted:
		owner = new Owner(interEnd, null, result.owner);
		mrw = null;
		activeReads = null;
		return null;
	}

	@Override
	public synchronized IntervalException checkLockable(IntervalMirror inter, LockMirror lock) {
		assert lock != null;
		
		PointMirror interStart = inter.end();
		State result = walkBack(interStart, inter);

		try {
			DataRace.Role role = new DataRace.LockRole(lock);
			if(lock != result.mrl) // either acquire same lock or...
				checkHappensAfterMostRecentWrite(interStart, inter, role, result);
			checkHappensAfterActiveReads(interStart, inter, role, result);
		} catch (IntervalException.DataRace err) {
			return err;
		}
		
		// lock is permitted:
		owner = new Owner(inter.end(), lock, result.owner);
		mrw = null;
		activeReads = null;
		return null;
	}

}