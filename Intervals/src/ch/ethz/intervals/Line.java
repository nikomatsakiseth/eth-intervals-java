package ch.ethz.intervals;


/**
 * {@code Line} represents a straight-line in the interval
 * diagram.  There is one per interval.  Lines are a very
 * minimal class containing only what information is needed
 * to answer {@code hb} queries and to drive the race detection
 * machinery.  This is intended to guarantee that if users
 * keep a pointer to a {@link Point} or {@link Interval} object,
 * then they will only keep successors of that object alive.  
 */
final class Line {
	static final Line rootLine = new Line();
	
	final Point bound;
	final int depth;                  /** Depth in line tree. */
	
	private Current unscheduled;

	private LockList locks;

	private Line() {
		this.bound = null;
		this.depth = 0;
	}
	
	Line(Current unscheduled, Point bound) {
		this.unscheduled = unscheduled;
		this.bound = bound;
		this.depth = bound.line.depth + 1;
	}

	boolean isUnscheduled(Current current) {
		return unscheduled == current;
	}
	
	/** Note: Safety checks apply!  See {@link Current#checkCanAddDep(Point)} */ 
	void addLock(Lock lock, boolean exclusive) {
		LockList list = new LockList(lock, exclusive, null);
		synchronized(this) {
			list.next = locks;
			locks = list;
		}
	}
	
	Point[] bounds() {
		Point[] bounds = new Point[depth];
		Point b = bound;
		for(int i = depth - 1; i >= 0; i--) {
			bounds[i] = b;
			b = b.line.bound;
		}
		return bounds;
	}
	
	synchronized LockList locksSync() {
		return locks;
	}
	
	LockList locksUnsync() {
		return locks;
	}
	
	boolean isScheduled() {
		return unscheduled == null;
	}
	
	void clearUnscheduled() {
		unscheduled = null;
	}

	/**
	 * True if {@code this} will hold the lock {@code lock}
	 * when it executes.
	 * @param interval TODO
	 * @param lock TODO
	 */
	public final boolean holdsLock(Lock lock) {
		for(LockList ll = locksSync(); ll != null; ll = ll.next)
			if(ll.lock == lock)
				return true;
		return false;
	}
	
}
