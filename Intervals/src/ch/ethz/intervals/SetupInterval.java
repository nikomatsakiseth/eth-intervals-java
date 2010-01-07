package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.addHb;

/** Base class for tasks with an extended setup period.  
 *  @see #setup(Point, Interval)  */
public abstract class SetupInterval extends Interval {

	public SetupInterval(Dependency dep) {
		super(dep);
	}

	@Override
	public void run() {
		final Point parentEnd = end;
		new Interval(parentEnd) {
			protected void run() {				
				final Point setupEnd = end;
				
				class WorkerInterval extends Interval {
					
					public WorkerInterval() {
						super(parentEnd);
						addHb(setupEnd, start);
					}

					@Override
					protected void run() {
					}
					
					@Override
					public String toString() {
						return "worker";
					}
					
				}
				
				WorkerInterval worker = new WorkerInterval();
				worker.schedule();
				
				setup(setupEnd, worker);
			}
			
			@Override
			public String toString() {
				return "setup";
			}
		};
	}
	
	/**
	 * Overriden to define the behavior of this task.  This method defines the
	 * setup period.  The worker interval parameter will not execute until after
	 * the end of the setup period, so you can create subintervals of
	 * {@code worker} that will not run until this method returns.
	 * 
	 * Normally, however, this guarantee can be obtained in a more straight-forward
	 * fashion by any task simply by not invoking {@link Intervals#schedule()}
	 * until all setup is done (or allowing the runtime to invoke it automatically
	 * for you).
	 */
	protected abstract void setup(Point setupEnd, Interval worker);

}
