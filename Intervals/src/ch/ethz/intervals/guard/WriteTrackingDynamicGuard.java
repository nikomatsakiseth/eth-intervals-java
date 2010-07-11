package ch.ethz.intervals.guard;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.IntervalException.DataRace;
import ch.ethz.intervals.IntervalException.DataRace.Role;
import ch.ethz.intervals.Lock;
import ch.ethz.intervals.Point;

/**
 * Dynamic guards monitor field accesses dynamically to guarantee 
 * that no race conditions occur.  They require that all writes
 * to data sharing the same dynamic guard <em>happen before</em>
 * all other writes as well as any reads.  
 * 
 * <p>This class serves as an abstract base class for other dynamic
 * checkers. It keeps track of the active and most recent intervals
 * to write or lock as well as any active readers.  
 * 
 * <p>This class is intended to be customized 
 * with respect to the amount of detail it tracks on the active readers:
 * the type parameter {@code R} defines the type of the state that is
 * being used, and several abstract methods manipulate the state to add
 * readers or check for conflicts.  Note that {@code R} is always 
 * reset to {@code null} to indicate that there are no readers at all. 
 */
abstract class WriteTrackingDynamicGuard<R> implements DynamicGuard {
	
	final static class Owner {
		final Point end;
		final Lock lock;
		final Owner prev;
		
		Owner(Point bound, Lock lock, Owner prev) {
			this.end = bound;
			this.lock = lock;
			this.prev = prev;
		}
		
		public boolean boundsOrEquals(Interval inter) {
			return (end == null) || inter.getEnd() == end || inter.getEnd().isBoundedBy(end);
		}
	}

	static final Owner rootOwner = new Owner(null, null, null);
	
	private final String name;
	
	protected static final class State<R> {
		Owner owner;
		Point mrw;
		Lock mrl;
		R activeReads;
	}
	
	/** current owner */
	Owner owner = WriteTrackingDynamicGuard.rootOwner;
	
	/** end of most recent write within current owner */
	Point mrw = null;
	
	/** end of potentially active reads within current owner */
	R activeReads = null;
	
	public WriteTrackingDynamicGuard() {
		this(null);
	}
	
	public WriteTrackingDynamicGuard(String name) {
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
	private State<R> walkBack(Point mr, Interval inter) {
		State<R> result = new State<R>();
		if(owner.boundsOrEquals(inter)) {
			result.owner = owner;
			result.mrw = mrw;
			result.mrl = null;
			result.activeReads = activeReads;			
		} else {
			WriteTrackingDynamicGuard.Owner o = owner;
			do {
				result.mrw = o.end;
				result.mrl = o.lock;
				o = o.prev;
			} while(!o.boundsOrEquals(inter));			
			result.activeReads = null;
			result.owner = o;
		}
		return result;
	}
	
	private Role mrRole(State<R> result) {
		return (result.mrl != null ? new DataRace.LockRole(result.mrl) : DataRace.WRITE);
	}
	
	private void checkHappensAfterMostRecentWrite(
			final Point mr,
			final Interval inter, 
			final Role interRole,
			State<R> result) 
	{
		if(result.mrw != null && !result.mrw.hbeq(mr))
			throw new IntervalException.DataRace(this, interRole, inter, mrRole(result), result.mrw);
	}

	/** Adds an active reader to the read set.
	 * 
	 *  @code reads the previous set of readers.  This value may be modified in place
	 *  if desired.
	 *  @code interEnd the bound of the reader to add to the read set
	 *  
	 *  @returns the new read set */
	protected abstract R addActiveReadBoundedBy(R reads, Point interEnd);

	/** Checks whether the given interval happens after all the reads represented
	 *  by the read set {@code reads}.  
	 *  
	 *  @param mr the most recent point within {@code inter} that has occurred
	 *  @param inter the accessing interval
	 *  @param interRole the role of the accessing interval (read, write, etc)
	 *  @param reads the set of reads
	 *  
	 *  @throws IntervalException if an error is detected
	 */
	protected abstract void checkHappensAfterActiveReads(
			final Point mr,
			final Interval inter, 
			final Role interRole,
			final R reads);
	
	@Override
	public synchronized IntervalException checkReadable(Point mr, Interval inter) {
		Point interEnd = inter.getEnd();
		
		// Read by owner, ok.
		if(owner.end == interEnd)
			return null;
		
		State<R> result = walkBack(mr, inter);		
		try {
			checkHappensAfterMostRecentWrite(mr, inter, DataRace.READ, result);
		} catch (IntervalException.DataRace err) {
			return err;
		}
		
		// read is permitted:
		owner = result.owner;
		mrw = result.mrw;
		activeReads = addActiveReadBoundedBy(result.activeReads, interEnd);
		return null;
	}
	
	@Override
	public synchronized IntervalException checkWritable(final Point mr, final Interval inter) {
		Point interEnd = inter.getEnd();
		
		// Write by owner, ok.
		if(owner.end == interEnd)
			return null;
		
		State<R> result = walkBack(mr, inter);
		
		try {
			checkHappensAfterMostRecentWrite(mr, inter, DataRace.WRITE, result);
			checkHappensAfterActiveReads(mr, inter, DataRace.WRITE, result.activeReads);
		} catch (IntervalException.DataRace err) {
			return err;
		}
		
		// write is permitted:
		owner = new WriteTrackingDynamicGuard.Owner(interEnd, null, result.owner);
		mrw = null;
		activeReads = null;
		return null;
	}

	@Override
	public synchronized IntervalException checkLockable(Interval inter, Lock lock) {
		assert lock != null;
		
		Point interStart = inter.getEnd();
		State<R> result = walkBack(interStart, inter);

		try {
			DataRace.Role role = new DataRace.LockRole(lock);
			if(lock != result.mrl) // either acquire same lock or...
				checkHappensAfterMostRecentWrite(interStart, inter, role, result);
			checkHappensAfterActiveReads(interStart, inter, role, result.activeReads);
		} catch (IntervalException.DataRace err) {
			return err;
		}
		
		// lock is permitted:
		owner = new WriteTrackingDynamicGuard.Owner(inter.getEnd(), lock, result.owner);
		mrw = null;
		activeReads = null;
		return null;
	}

}
