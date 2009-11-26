package ch.ethz.intervals;

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
