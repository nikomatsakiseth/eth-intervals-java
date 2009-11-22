package ch.ethz.intervals;

import java.util.concurrent.CountDownLatch;

/**
 * Async. points are used to model asynchronous, user-defined events
 * which the scheduler should wait for.  They are similar to a {@link CountDownLatch}:
 * when an async. point is constructed (via {@link Intervals#asyncPoint(Point, int)})
 * they have a certain number of outstanding references.  The user must invoke
 * {@link #arrive(int)} with the correct total count in order to cause the interval
 * to begin execution.  
 */
public interface AsyncPoint extends Point {
	/**
	 * Causes the async. point to consider that {@code count} of its outstanding
	 * references have been resolved. Once the number of outstanding references
	 * drops to zero, the point will be considered by the scheduler as ready to
	 * occur.
	 *  
	 * @param count the number of outstanding references that have completed
	 */
	public void arrive(int count);
}
