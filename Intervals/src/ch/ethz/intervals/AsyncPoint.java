package ch.ethz.intervals;

import java.util.concurrent.CountDownLatch;

/**
 * Asynchronous points are used to model asynchronous, user-defined events
 * which the scheduler should wait for.  They are similar to a {@link CountDownLatch}:
 * when an async. point is constructed (via {@link Intervals#asyncPoint(Point, int)})
 * they have a certain number of outstanding references.  The user must invoke
 * {@link #trigger(int)} with the correct total count in order to cause the interval
 * to begin execution.  
 * 
 * <b>Warning:</b> if you forget to invoke {@link #trigger(int)}, the program
 * will eventually deadlock.
 */
public interface AsyncPoint extends Point {
	/**
	 * Causes the async. point to consider that {@code count} of its outstanding
	 * references have been resolved. Once the number of outstanding references
	 * drops to zero, the point will be considered by the scheduler as ready to
	 * occur.  Extra calls after the count has reached zero have no effect. 
	 *  
	 * @param count the number of outstanding references that have completed.
	 * Must be greater than or equal to 0.
	 */
	public void trigger(int count);
}
