package ch.ethz.intervals.mirror;

import ch.ethz.intervals.guard.Guard;
import ch.ethz.intervals.impl.LockImpl;

/** 
 * Mirror class representing locks.  {@link Guard} implementations should
 * use this class in place of {@link LockImpl}.
 */
public interface Lock {
}
