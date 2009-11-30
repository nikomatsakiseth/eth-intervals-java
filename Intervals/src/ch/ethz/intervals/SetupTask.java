package ch.ethz.intervals;

public abstract class SetupTask extends AbstractTask {

	@Override
	public void run(final Point parentEnd) {
		Intervals.intervalWithBound(parentEnd, new AbstractTask() {
			
			@Override
			public String toString() {
				return "setup";
			}
			
			@Override
			public void run(Point setupEnd) {
				Interval worker = Intervals.intervalWithBound(parentEnd, Intervals.namedTask("worker"));
				Intervals.addHb(setupEnd, worker.start());
				setup(setupEnd, worker);
			}
			
		});
	}
	
	protected abstract void setup(Point setupEnd, Interval worker);

}
