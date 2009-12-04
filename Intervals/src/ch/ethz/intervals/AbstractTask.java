package ch.ethz.intervals;

/** Recommended base class for interval tasks. */
public abstract class AbstractTask implements Task {

	@Override
	public abstract void run(Point currentEnd);
	
	/** By default, no dependencies. */
	public void addDependencies(Interval inter) {}

}
