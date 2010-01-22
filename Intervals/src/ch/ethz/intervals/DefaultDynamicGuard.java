package ch.ethz.intervals;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import ch.ethz.intervals.IntervalException.DataRace.Role;
import ch.ethz.intervals.guard.DynamicGuard;
import ch.ethz.intervals.guard.Guard;
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
	private final List<PointMirror> writes = new ArrayList<PointMirror>();
	private final Map<PointMirror, LockMirror> locks = new HashMap<PointMirror, LockMirror>();
	private final Set<PointMirror> reads = new HashSet<PointMirror>();
	
	public DefaultDynamicGuard(String name) {
		this.name = name;
	}
	
	@Override public String toString() {
		return name;
	}
	
	/** Indicates that either a is b or else a is suspended/inactive 
	 * 	while b is active. a is always the end of an
	 *  interval, but b may be either the start or end. */
	private boolean admits(PointMirror a, PointMirror mr, IntervalMirror inter) {
		return (a == inter.end() || inter.end().isBoundedBy(a) || a.hbeq(mr));
	}
	
	@Override
	public synchronized Void checkReadable(PointMirror mr, IntervalMirror inter) {
		for(PointMirror w : writes) {
			if(!admits(w, mr, inter))
				throw new IntervalException.DataRace(this, Role.READ, inter, Role.WRITE, w);
		}
		reads.add(inter.end());
		return null;
	}
	

	@Override
	public synchronized Void checkWritable(PointMirror mr, IntervalMirror inter) {
		int writeSize = writes.size();
		if(writeSize >= 1) {			
			PointMirror w = writes.get(writeSize - 1);			
			if(!admits(w, mr, inter))
				throw new IntervalException.DataRace(this, Role.WRITE, inter, Role.WRITE, w);				
		}
		
		for(PointMirror r : reads) {
			if(!admits(r, mr, inter))
				throw new IntervalException.DataRace(this, Role.WRITE, inter, Role.READ, r);
		}
		
		writes.add(inter.end());
		
		return null;
	}
	
	@Override
	public synchronized Void checkLockable(IntervalMirror inter, LockMirror lock) {
		if(!inter.locks(lock))
			throw new IntervalException.LockNotHeld(lock, inter);
		
		int writeSize = writes.size();
		if(writeSize >= 1) {
			PointMirror w = writes.get(writeSize - 1);			
			if(!admits(w, inter.start(), inter) && !holdsLock(w, lock))
				throw new IntervalException.DataRace(this, Role.LOCK, inter, Role.WRITE, w);			
		}
		
		if(locks.get(inter.end()) != null && locks.get(inter.end()) != lock)
			throw new IntervalException.DataRace(this, Role.LOCK, inter, Role.LOCK, inter.end());
		
		for(PointMirror r : reads) {
			if(!admits(r, inter.start(), inter) && !holdsLock(r, lock))
				throw new IntervalException.DataRace(this, Role.LOCK, inter, Role.READ, r);
		}
		
		writes.add(inter.end());
		locks.put(inter.end(), lock);
		return null;
	}

	private boolean holdsLock(PointMirror end, LockMirror lock) {
		if(locks.get(end) == lock) 
			return true;
		for(PointMirror w1 : writes) 
			if(end.isBoundedBy(w1) && locks.get(w1) == lock)
				return true;
		return false;
	}
	

}