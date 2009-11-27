package ch.ethz.intervals;

/**
 * Embodies a task which requires an initial "setup" phase that 
 * creates the interval structure that performs
 * the actual computation.  The method {@link SetupTask#setup(Interval, Interval)}
 * is invoked with two intervals: the first is the current,
 * setup interval.  The second is a sibling which will not start
 * until the setup interval has completed.  The setup interval can therefore
 * create new work during the next interval using
 * {@link #intervalDuring(Interval)}, safe in the knowledge that none
 * of these tasks will execute until the setup interval is complete.  
 * 
 * @param <R> The result of the setup interval (typically {@link Void})
 * @param setupTask The setup task.
 * @return the value returned by the setup interval, 
 * once the mutual bound of the setup interval and its sibling
 * has finished 
 */
public abstract class SetupTask extends AbstractTask {	
	public abstract void setup(Point setupEnd, Interval worker);
	
	public final void run(final Point parentEnd) {
		Intervals.intervalWithBound(parentEnd)
			.schedule(new Task() {
				public void run(final Point setupEnd) {
					Interval worker = Intervals.intervalWithBound(parentEnd)
						.startAfter(setupEnd)
						.schedule(Intervals.emptyTask);
					setup(setupEnd, worker);
				}
			});	
	}
}
