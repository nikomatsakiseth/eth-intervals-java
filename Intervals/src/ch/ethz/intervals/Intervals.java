package ch.ethz.intervals;

import ch.ethz.intervals.guard.DynamicGuard;
import ch.ethz.intervals.guard.Guard;
import ch.ethz.intervals.impl.ContextImpl;
import ch.ethz.intervals.task.AbstractTask;
import ch.ethz.intervals.task.ResultTask;

/** 
 * Static convenience methods.  Also provides a means to
 * access the current context for a given piece of code. */
public class Intervals {
	
	/**
	 * If set to false, disables all safety checks against
	 * cycles or race conditions.  
	 */
	public static final boolean SAFETY_CHECKS = true;	

	/** 
	 * Returns the current context for the current thread. 
	 * If the current thread is not running in an interval,
	 * will return a default context object. */
	public static Context context() {
		// XXX Move to some thread-local system.
		return ContextImpl.intervals;
	}
	
	/** 
	 * Creates a new lock in the current context.
	 * 
	 * @see Context#lock(String) */
	public static Lock lock(String name) {
		return context().lock(name);
	}
	
	public static <T> ScopedVar<T> scopedVar(T defaultValue) {
		return context().scopedVar(defaultValue);
	}
	
	/** 
	 * Returns {@link Interval#getStart()} unless {@code i} is null, 
	 * in which case just returns {@code null}. */
	public static Point getStart(Interval i) {
		if(i == null)
			return null;
		return i.getStart();
	}
	
	/** 
	 * Returns {@link Interval#getEnd()} unless {@code i} is null, 
	 * in which case just returns {@code null}. */
	public static Point getEnd(Interval i) {
		if(i == null)
			return null;
		return i.getEnd();
	}
	
	/** 
	 * Equivalent of {@code addHb(from.getEnd(), to.getStart()}
	 * @see Point#addHb(Point) */
	public static void addHb(Interval from, Interval to) {
		from.getEnd().addHb(to.getStart());
	}
	
	/** 
	 * Equivalent of {@code addHb(from.getEnd(), to)}
	 * @see Point#addHb(Point)
	 */
	public static void addHb(Interval from, Point to) {
		from.getEnd().addHb(to);
	}
	
	/** 
	 * Equivalent of {@code addHb(from, to.getStart()}
	 * @see Point#addHb(Point)
	 */
	public static void addHb(Point from, Interval to) {
		from.addHb(to.getStart());
	}
	
	/** 
	 * Equivalent to {@code from.addHb(to)}
	 * Here for consistency with other {@code addHb()} methods.
	 * @see Point#addHb(Point) */
	public static void addHb(Point from, Point to) {
		from.addHb(to);
	}

	/** 
	 * Invokes {@link #addHb(Interval, Interval)} if both
	 * {@code from} and {@code to} are not null. */
	public static void addHbIfNotNull(Interval from, Interval to) {
		if(from != null && to != null)
			addHb(from, to);
	}	
	
	/** 
	 * Invokes {@link #addHb(Point, Interval)} if both
	 * {@code from} and {@code to} are not null. */
	public static void addHbIfNotNull(Point from, Interval to) {
		if(from != null && to != null)
			addHb(from, to);
	}	
	
	/** 
	 * Invokes {@link #addHb(Interval, Point)} if both
	 * {@code from} and {@code to} are not null. */
	public static void addHbIfNotNull(Interval from, Point to) {
		if(from != null && to != null)
			addHb(from, to);
	}	
	
	/** 
	 * Invokes {@link #addHb(Point, Point)} if both
	 * {@code from} and {@code to} are not null. */
	public static void addHbIfNotNull(Point from, Point to) {
		if(from != null && to != null)
			addHb(from, to);
	}	
	
	/**
	 * Creates a new interval which executes during the current interval.
	 * This interval will execute {@code task}.  This function does not
	 * return until the new interval has completed.
	 * 
	 * <b>Note:</b> Exceptions that occur in {@code task} are 
	 * wrapped in {@link RethrownException} and rethrown immediately.
	 * Exceptions never propagate to the current interval. */
	public static void inline(final Task task) {
		InlineInterval inter = context().unexecutedInline(task);
		inter.execute();
	}
	
	/** 
	 * Like {@link #inline(Task)} but returns the result of the
	 * task afterwards. */
	public static <R> R inline(final ResultTask<R> task) {
		inline((Task)task);
		return task.getResult();
	}
	
	/** 
	 * Invokes {@link Context#checkWritable(Guard)} on the
	 * current context.  Intended to be used in asserts. */
	public static boolean checkWritable(Guard guard) {
		return context().checkWritable(guard);
	}

	/** 
	 * Invokes {@link Context#checkReadable(Guard)} on the
	 * current context.  Intended to be used in asserts. */
	public static boolean checkReadable(Guard guard) {
		return context().checkReadable(guard);
	}

	/** 
	 * Invokes {@link Context#makeWritable(Guard)} on the
	 * current context.  Intended to be used in asserts. */
	public static void makeWritable(DynamicGuard guard) {
		context().makeWritable(guard);
	}

	/** 
	 * Invokes {@link Context#makeReadable(Guard)} on the
	 * current context.  Intended to be used in asserts. */
	public static void makeReadable(DynamicGuard guard) {
		context().makeReadable(guard);
	}

	/** 
	 * Invokes {@link Context#makeFinal(Guard)} on the
	 * current context.  Intended to be used in asserts. */
	public static void makeFinal(DynamicGuard guard) {
		context().makeFinal(guard);
	}

	/**
	 * This function does not return until 
	 * {@code toJoin} has completed.  This is
	 * the equivalent of creating an inline 
	 * interval X where {@code inter.end -> X.start},
	 * but potentially optimized.  
	 * 
	 * If this causes a cycle, will throw 
	 * a {@link IntervalException.Cycle}.
	 * 
	 * If {@code toJoin} failed with uncaught exceptions,
	 * then will throw a {@link RethrownException}
	 * containing those errors. 
	 * 
	 * @param toJoin the interval to join */
	public static void join(final Interval toJoin) {
		if(!toJoin.getEnd().didOccurWithoutError()) {
			Intervals.inline(new AbstractTask("join:"+toJoin.toString()) {
				@Override public void attachedTo(Interval inter) {
					super.attachedTo(inter);
					toJoin.getEnd().addHb(inter.getStart());
				}

				@Override public void run(Interval current) throws Exception {
				}
			});
		}
	}
	

}
