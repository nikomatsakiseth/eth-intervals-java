package ch.ethz.intervals;

import ch.ethz.intervals.guard.ReadTrackingDynamicGuard;
import ch.ethz.intervals.guard.Guard;
import ch.ethz.intervals.impl.IntervalImpl;
import ch.ethz.intervals.impl.PointImpl;
import ch.ethz.intervals.mirror.Interval;
import ch.ethz.intervals.mirror.Lock;
import ch.ethz.intervals.mirror.Point;


/** Base class for all exceptions thrown by the interval runtime. */
@SuppressWarnings("serial")
public abstract class IntervalException extends RuntimeException {

	public IntervalException() {
	}

	public IntervalException(String arg0) {
		super(arg0);
	}

	public IntervalException(Throwable arg0) {
		super(arg0);
	}

	public IntervalException(String arg0, Throwable arg1) {
		super(arg0, arg1);
	}
	
	/** Indicates that the user's action would have created a cycle in the
	 *  scheduler graph. */
	public static class Cycle extends IntervalException {
		private static final long serialVersionUID = 2242825867497429060L;
		public final Point from, to;

		public Cycle(Point from, Point to) {
			this.from = from;
			this.to = to;
		}
		
		public String toString() {
			return String.format(
					"Adding an edge from %s to %s would create a cycle",
					from, to);
		}		
		
	}
	
	public static class MustBeBoundedBy extends IntervalException {
		private static final long serialVersionUID = -3545763123904421907L;
		
		public final Point bound;
		public final Point point;
		
		public MustBeBoundedBy(Point bound, Point point) {
			this.bound = bound;
			this.point = point;
		}
		
		public String toString() {
			return point + " must be bounded by " + bound;
		}
		
	}
	
	public static class MustBeCurrent extends IntervalException {
		public final Interval inter;
		
		public MustBeCurrent(Interval inter) {
			this.inter = inter;
		}

		public String toString() {
			return String.format(
					"%s must be the current interval to perform this operation", 
					inter);
		}		
		
	}

	/**
	 * Indicates that the given method cannot be called from the root interval.
	 */
	public static class NotInRootInterval extends RuntimeException {
		private static final long serialVersionUID = 6938307942423182780L;
		
		public String toString() {
			return "this operation cannot be performed in the root interval";
		}	
	}
	
	public static class NotSubinterval extends IntervalException {
		public final Interval current;
		public final Interval reqdParent;
		
		public NotSubinterval(Interval current, Interval reqdParent) {
			this.current = current;
			this.reqdParent = reqdParent;
		}

		public String toString() {
			return String.format(
					"%s must be a subinterval of %s to perform this operation", 
					current, reqdParent);
		}		
	}

	public static class LockNotHeld extends IntervalException {
		public final Lock lock;
		public final Interval inter;

		public LockNotHeld(Lock lock, Interval inter) {
			this.lock = lock;
			this.inter = inter;
		}

		public String toString() {
			return String.format(
					"%s must hold the lock %s to perform this operation", 
					inter, lock);
		}		
	}
	
	public static class MustHappenBefore extends IntervalException {
		public final Point before;
		public final Point after;

		public MustHappenBefore(Point before, Point after) {
			this.before = before;
			this.after = after;
		}

		public String toString() {
			return String.format(
					"%s must happen before %s to perform this operation", 
					before, after);
		}		
	}
	
	/** Indicates that a data race was detected on a {@link ReadTrackingDynamicGuard} */
	public static class DataRace extends IntervalException {
		public abstract static class Role { }
		private static class NamedRole extends Role {
			private final String name;
			NamedRole(String name) {
				this.name = name;
			}
			@Override public String toString() { return name; }
		}
		public static final Role READ = new NamedRole("READ");
		public static final Role WRITE = new NamedRole("WRITE");
		public static class LockRole extends Role {
			public final Lock lock;
			
			public LockRole(Lock lock) {
				this.lock = lock;
			}

			@Override public String toString() { return "LOCK("+lock+")"; }
		}

		public final Guard dg;      			/** Guard on which the race occurred. */
		
		public final Role interloperRole;  		/** Kind of access which failed. */
		public final Interval interloper; /** Interval performing failed access. */

		public final Role ownerRole;       		/** Current role of {@link #dg} */
		public final Point ownerBound;    /** Bound when current role ends. */
		
		public DataRace(
				Guard dg,
				Role interloperRole,
				Interval interloper,
				Role ownerRole,
				Point ownerBound) 
		{
			this.dg = dg;
			this.interloperRole = interloperRole;
			this.interloper = interloper;
			this.ownerRole = ownerRole;
			this.ownerBound = ownerBound;
		}

		@Override public String toString() {
			return String.format(
					"%s attempted to %s %s but it is currently in role %s until %s",
					interloper, interloperRole, dg, ownerRole, ownerBound
					);
		}
	}

	public static class InternalError extends IntervalException {
		public InternalError(String arg0) {
			super(arg0);
		}

		public InternalError(String arg0, Throwable arg1) {
			super(arg0, arg1);
		}
	}

	public static class AlreadyScheduled extends IntervalException {
		public final Interval inter;

		public AlreadyScheduled(IntervalImpl inter) {
			this.inter = inter;
		}
		
		@Override public String toString() {
			return inter + " already scheduled";
		}
	}
	
	public static class CannotBeLockedBy extends IntervalException {
		public final Guard guard;
		public final Lock lock;

		public CannotBeLockedBy(Guard guard, Lock lock) {
			this.guard = guard;
			this.lock = lock;
		}
		
		@Override public String toString() {
			return "Guard " + guard + " cannot be locked by " + lock;
		}
	}
	
	public static class ParPhaseCompleted extends IntervalException {
		public final Interval interval;
		
		public ParPhaseCompleted(Interval interval) {
			this.interval = interval;
		}
		
		@Override public String toString() {
			return "The parallel phase of " + interval + " has completed.  No new children can be created.";
		}
	}



}
