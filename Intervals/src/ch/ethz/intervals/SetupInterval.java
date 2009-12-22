package ch.ethz.intervals;

/** Base class for tasks with an extended setup period.  
 *  @see {@link #setup(Point, Interval)}.  */
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
				Interval worker = new Interval(parentEnd) {
					@Override
					protected void addDependencies() {
						super.addDependencies();
						Intervals.addHb(setupEnd, start());
					}

					@Override
					protected void run() {						
					}
					
					@Override
					public String toString() {
						return "worker";
					}
				};
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
