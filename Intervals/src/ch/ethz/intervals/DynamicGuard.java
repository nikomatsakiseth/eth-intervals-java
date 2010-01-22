package ch.ethz.intervals;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import ch.ethz.intervals.IntervalException.DataRace.Role;


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
	
	private final List<Point> writes = new ArrayList<Point>();
	private final Map<Point, Lock> locks = new HashMap<Point, Lock>();
	private final Set<Point> reads = new HashSet<Point>();
	
	public DynamicGuard() {
		super();
	}

	public DynamicGuard(String name) {
		super(name);
	}

	@Override
	IntervalException checkLockableByReturningException(Interval inter) {
		try {
			checkLockableBy(inter.start, inter, this);
			return null;
		} catch (IntervalException err) {
			return err;
		}
	}

	@Override
	public boolean checkReadable() {
		Current current = Current.get();
		if(current.inter == null)
			throw new NotInRootIntervalException(); // later we could allow this maybe
		checkReadableBy(current.mr, current.inter);
		return true;
	}

	@Override
	public boolean checkWritable() {
		Current current = Current.get();
		if(current.inter == null)
			throw new NotInRootIntervalException(); // later we could allow this maybe
		checkWritableBy(current.mr, current.inter);
		return true;
	}
	
	public boolean checkLockable(Lock lock) {
		Current current = Current.get();
		if(current.inter == null)
			throw new NotInRootIntervalException(); // later we could allow this maybe
		checkLockableBy(current.mr, current.inter, lock);
		return true;		
	}
	
	/** Indicates that either a is b or else a is suspended/inactive 
	 * 	while b is active. a is always the end of an
	 *  interval, but b may be either the start or end. */
	private boolean admits(Point a, Point mr, Interval inter) {
		return (a == inter.end || inter.end.isBoundedBy(a) || a.hbeq(mr));
	}
	
	private synchronized Void checkReadableBy(Point mr, Interval inter) {
		for(Point w : writes) {
			if(!admits(w, mr, inter))
				throw new IntervalException.DataRace(this, Role.READ, inter, Role.WRITE, w);
		}
		reads.add(inter.end);
		return null;
	}
	
	private synchronized Void checkWritableBy(Point mr, Interval inter) {
		int writeSize = writes.size();
		if(writeSize >= 1) {			
			Point w = writes.get(writeSize - 1);			
			if(!admits(w, mr, inter))
				throw new IntervalException.DataRace(this, Role.WRITE, inter, Role.WRITE, w);				
		}
		
		for(Point r : reads) {
			if(!admits(r, mr, inter))
				throw new IntervalException.DataRace(this, Role.WRITE, inter, Role.READ, r);
		}
		
		writes.add(inter.end);
		
		return null;
	}
	
	private synchronized Void checkLockableBy(Point mr, Interval inter, Lock lock) {
		if(!inter.locks(lock))
			throw new IntervalException.LockNotHeld(lock, inter);
		
		int writeSize = writes.size();
		if(writeSize >= 1) {
			Point w = writes.get(writeSize - 1);			
			if(!admits(w, mr, inter) && !holdsLock(w, lock))
				throw new IntervalException.DataRace(this, Role.LOCK, inter, Role.WRITE, w);			
		}
		
		if(locks.get(inter.end) != null && locks.get(inter.end) != lock)
			throw new IntervalException.DataRace(this, Role.LOCK, inter, Role.LOCK, inter.end);
		
		for(Point r : reads) {
			if(!admits(r, mr, inter) && !holdsLock(r, lock))
				throw new IntervalException.DataRace(this, Role.LOCK, inter, Role.READ, r);
		}
		
		writes.add(inter.end);
		locks.put(inter.end, lock);
		return null;
	}

	private boolean holdsLock(Point end, Lock lock) {
		if(locks.get(end) == lock) 
			return true;
		for(Point w1 : writes) 
			if(end.isBoundedBy(w1) && locks.get(w1) == lock)
				return true;
		return false;
	}
	

}