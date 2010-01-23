package ch.ethz.intervals;

import ch.ethz.intervals.guard.ReadTrackingDynamicGuard;
import ch.ethz.intervals.guard.Guard;
import ch.ethz.intervals.mirror.IntervalMirror;
import ch.ethz.intervals.mirror.LockMirror;
import ch.ethz.intervals.mirror.PointMirror;


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
	
	public static class NotSubinterval extends IntervalException {
		public final IntervalMirror current;
		public final IntervalMirror reqdParent;
		
		public NotSubinterval(IntervalMirror current, IntervalMirror reqdParent) {
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
		public final LockMirror lock;
		public final IntervalMirror inter;

		public LockNotHeld(LockMirror lock, IntervalMirror inter) {
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
		public final PointMirror before;
		public final PointMirror after;

		public MustHappenBefore(PointMirror before, PointMirror after) {
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
			public final LockMirror lock;
			
			public LockRole(LockMirror lock) {
				this.lock = lock;
			}

			@Override public String toString() { return "LOCK("+lock+")"; }
		}

		public final Guard dg;      			/** Guard on which the race occurred. */
		
		public final Role interloperRole;  		/** Kind of access which failed. */
		public final IntervalMirror interloper; /** Interval performing failed access. */

		public final Role ownerRole;       		/** Current role of {@link #dg} */
		public final PointMirror ownerBound;    /** Bound when current role ends. */
		
		public DataRace(
				Guard dg,
				Role interloperRole,
				IntervalMirror interloper,
				Role ownerRole,
				PointMirror ownerBound) 
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
		public final IntervalMirror inter;

		AlreadyScheduled(Interval inter) {
			this.inter = inter;
		}
		
		@Override public String toString() {
			return inter + " already scheduled";
		}
	}
	
	public static class CannotBeLockedBy extends IntervalException {
		public final Guard guard;
		public final LockMirror lock;

		CannotBeLockedBy(Guard guard, LockMirror lock) {
			this.guard = guard;
			this.lock = lock;
		}
		
		@Override public String toString() {
			return "Guard " + guard + " cannot be locked by " + lock;
		}
	}

}
