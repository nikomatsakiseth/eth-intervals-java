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
	static final Line rootLine = new Line(null);
	
	private Current unscheduled;

	Line(Current unscheduled) {
		this.unscheduled = unscheduled;
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
