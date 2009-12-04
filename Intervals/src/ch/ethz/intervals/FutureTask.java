package ch.ethz.intervals;

/**
 * A useful base class which saves the return value of the
 * {@link #generate(Point)} method.  It can later
 * be accessed by invoking {@link #result()}.  To avoid
 * race conditions, you should ensure that any task which
 * invokes {@link #result()} executes after the end of
 * the interval associated with this task.
 */
public abstract class FutureTask<R> extends AbstractTask {	
	private R result;
		
	public R result() {
		return result;
	}
	
	public abstract R generate(Point currentEnd);
	
	public final void run(Point currentEnd) {
		result = generate(currentEnd);
	}
}
