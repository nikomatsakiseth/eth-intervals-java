/**
 * 
 */
package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.intervalWithBound;

final class SetupTaskWrapper<R> implements Task<R> {
	private final SetupTask<R> setupTask;

	SetupTaskWrapper(SetupTask<R> setupTask) {
		this.setupTask = setupTask;
	}

	public R run(final Interval<R> parent) {
		return Intervals.blockingInterval(new Task<R>() {
			public R run(final Interval<R> setup) {
				Interval<Void> next = intervalWithBound(parent.end())
					.startAfter(setup.end())
					.schedule(Intervals.emptyTask);
				return setupTask.setup(setup, next);
			}
		});
	}
	
	public String toString() {
		return setupTask.toString();
	}
}