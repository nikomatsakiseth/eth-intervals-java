package ch.ethz.intervals;


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
	
	/** Indicates that a data race was detected on a {@link DynamicGuard} */
	public static class DataRace extends IntervalException {
		enum Role { READ, WRITE, LOCK, EMBED };

		public final DynamicGuard dg;      /** Guard on which the race occurred. */
		
		public final Role interloperRole;  /** Kind of access which failed. */
		public final Interval interloper;  /** Interval performing failed access. */

		public final Role ownerRole;       /** Current role of {@link #dg} */
		public final Point ownerBound;     /** Bound when current role ends. */
		
		public DataRace(
				DynamicGuard dg,
				ch.ethz.intervals.IntervalException.DataRace.Role interloperRole,
				Interval interloper,
				ch.ethz.intervals.IntervalException.DataRace.Role ownerRole,
				Point ownerBound) {
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
	
	public static class MustUnembed extends IntervalException {
		public final Guard embedded;
		public final Guard embeddedIn;
		
		public MustUnembed(Guard embedded, Guard embeddedIn) {
			this.embedded = embedded;
			this.embeddedIn = embeddedIn;
		}
		
		@Override public String toString() {
			return String.format(
					"%s must be unembedded from %s before it can be used",
					embedded, embeddedIn);
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

		AlreadyScheduled(Interval inter) {
			this.inter = inter;
		}
		
		@Override public String toString() {
			return inter + " already scheduled";
		}
	}
	
	public static class CannotEmbedInSelf extends IntervalException {
		public final Guard guard;

		public CannotEmbedInSelf(Guard guard) {
			this.guard = guard;
		}
		
		@Override public String toString() {
			return guard + " cannot be embedded in itself";
		}		
	}

}
