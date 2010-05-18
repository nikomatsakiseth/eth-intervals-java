package ch.ethz.intervals.task;

import ch.ethz.intervals.mirror.AsyncInterval;
import ch.ethz.intervals.mirror.Interval;

/** Base class for tasks with an extended setup period.  
 *  @see #setup(Interval, Interval)  */
public abstract class SetupTask extends AbstractTask {
	
	public SetupTask() {
		super();
	}

	public SetupTask(String name) {
		super(name);
	}

	/**
	 * Overriden to define the behavior of this task.  This method defines the
	 * setup period.  The worker interval parameter will not execute until after
	 * the end of the setup period, so you can create subintervals of
	 * {@code worker} that will not run until this method returns.
	 * 
	 * Normally, however, this guarantee can be obtained in a more straight-forward
	 * fashion by any task simply by not invoking {@link AsyncInterval#schedule()}
	 * until all setup is done (or allowing the runtime to invoke it automatically
	 * for you).
	 */
	protected abstract void setup(Interval setup, Interval worker);
	
	@Override
	public void run(Interval current) {
		final Interval worker = current.newAsyncChild(new EmptyTask(getName()+".worker"));
		
		final Interval setup = current.newAsyncChild(new AbstractTask(getName()+".setup") {
			@Override
			public void run(Interval current) {
				setup(current, worker);
			}
		});
		
		setup.getEnd().addHb(worker.getStart());
	}

}
