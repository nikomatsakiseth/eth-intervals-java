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
	static final Line rootLine = new Line(null, null);
	
	final Point bound;                /** {@code null} means root.end */
	final int depth;                  /** Depth in line tree. */
	
	private Current unscheduled;

	Line() { // the root line
		this.unscheduled = null;
		this.bound = null;
		this.depth = 0;
	}

	Line(Current unscheduled, Point bound) {
		this.unscheduled = unscheduled;
		this.bound = bound;
		this.depth = (bound == null ? 1 : bound.line.depth + 1);
	}

	boolean isUnscheduled(Current current) {
		return unscheduled == current;
	}
	
	boolean isScheduled() {
		return unscheduled == null;
	}
	
	void clearUnscheduled() {
		unscheduled = null;
	}
	
	public final String toString() {
		return "Line("+System.identityHashCode(this)+")";
	}	
	
}
