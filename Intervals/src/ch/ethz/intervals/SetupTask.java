package ch.ethz.intervals;

/** Base class for tasks with an extended setup period.  
 *  @see {@link #setup(Point, Interval)}.  */
public abstract class SetupTask extends AbstractTask {

	@Override
	public void run(final Point parentEnd) {
		Intervals.intervalWithBound(parentEnd, new AbstractTask() {
			
			@Override
			public String toString() {
				return "Setup";
			}
			
			@Override
			public void run(Point setupEnd) {
				Interval worker = Intervals.intervalWithBound(parentEnd, Intervals.namedTask("worker"));
				Intervals.addHb(setupEnd, worker.start());
				setup(setupEnd, worker);
			}
			
		});
	}
	
	/**
	 * Overriden to define the behavior of this task.  This method defines the
	 * setup period.  The worker interval parameter will not execute until after
	 * the end of the setup period, so you can use {@link Intervals#intervalDuring(Interval, Task)}
	 * to schedule work that will not run until this method returns.
	 * 
	 * Normally, however, this guarantee can be obtained in a more straight-forward
	 * fashion by any task simply by not invoking {@link Intervals#schedule()}
	 * until all setup is done (or allowing the runtime to invoke it automatically
	 * for you).
	 */
	protected abstract void setup(Point setupEnd, Interval worker);

}
