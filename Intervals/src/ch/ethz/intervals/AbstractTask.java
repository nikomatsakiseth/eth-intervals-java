package ch.ethz.intervals;

public abstract class AbstractTask implements Task {

	@Override
	public abstract void run(Point currentEnd);
	
}
