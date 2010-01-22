package ch.ethz.intervals;

/** 
 * Mirror class representing intervals.  {@link Guard} implementations should
 * use this class in place of {@link Interval}.
 */
public interface IntervalMirror {	
	public PointMirror start();
	public PointMirror end();
	public void addLock(LockMirror lock);
	public boolean locks(LockMirror lock);
}
