package ch.ethz.intervals;

import ch.ethz.intervals.guard.Guard;
import ch.ethz.intervals.impl.ContextImpl;
import ch.ethz.intervals.impl.IntervalImpl;
import ch.ethz.intervals.impl.PointImpl;
import ch.ethz.intervals.mirror.Context;
import ch.ethz.intervals.mirror.Interval;
import ch.ethz.intervals.mirror.Point;
import ch.ethz.intervals.mirror.Task;
import ch.ethz.intervals.task.ResultTask;

/** Static methods for creating and manipulating intervals. */
public class Intervals {
	
	/**
	 * If set to false, disables all safety checks against
	 * cycles or race conditions.  
	 */
	public static final boolean SAFETY_CHECKS = true;	

	/** 
	 * Returns the current context for the current thread. */
	public static Context context() {
		// XXX This is broken.
		return ContextImpl.intervals;
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
	public static void addHbIfNotNull(IntervalImpl from, IntervalImpl to) {
		if(from != null && to != null)
			addHb(from, to);
	}	
	
	/** 
	 * Invokes {@link #addHb(Point, Interval)} if both
	 * {@code from} and {@code to} are not null. */
	public static void addHbIfNotNull(PointImpl from, IntervalImpl to) {
		if(from != null && to != null)
			addHb(from, to);
	}	
	
	/** 
	 * Invokes {@link #addHb(Interval, Point)} if both
	 * {@code from} and {@code to} are not null. */
	public static void addHbIfNotNull(IntervalImpl from, PointImpl to) {
		if(from != null && to != null)
			addHb(from, to);
	}	
	
	/** 
	 * Invokes {@link #addHb(Point, Point)} if both
	 * {@code from} and {@code to} are not null. */
	public static void addHbIfNotNull(PointImpl from, PointImpl to) {
		if(from != null && to != null)
			addHb(from, to);
	}	
	
	/**
	 * Invokes {@link Context#inline(Task)} on the
	 * current context. */
	public static void inline(final Task task) {
		context().inline(task);
	}
	
	/** 
	 * Like {@link #inline(Task)} but returns the result of the
	 * task afterwards. */
	public static <R> R inline(final ResultTask<R> task) {
		context().inline(task);
		return task.getResult();
	}
	
	/** 
	 * Invokes {@link Context#checkWritable(Guard)} on the
	 * current context. */
	public static boolean checkWritable(Guard guard) {
		return context().checkWritable(guard);
	}

	/** 
	 * Invokes {@link Context#checkReadable(Guard)} on the
	 * current context. */
	public static boolean checkReadable(Guard guard) {
		return context().checkReadable(guard);
	}
}
