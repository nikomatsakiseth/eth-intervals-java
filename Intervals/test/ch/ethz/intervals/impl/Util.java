package ch.ethz.intervals.impl;

import org.junit.Assert;

import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.mirror.AsyncInterval;
import ch.ethz.intervals.mirror.Interval;
import ch.ethz.intervals.mirror.Point;
import ch.ethz.intervals.task.EmptyTask;
import ch.ethz.intervals.task.ResultTask;

public class Util {

	public abstract class ExpectedErrorTask extends ResultTask<Boolean> {
		
		final Class<?> expectedErrorClass;
		
		public ExpectedErrorTask(Class<?> expectedErrorClass) {
			this.expectedErrorClass = expectedErrorClass;
		}

		abstract void tryIt(Interval current) throws Exception;

		@Override
		protected Boolean compute(Interval current) throws Exception {
			try {
				tryIt(current);
				return false; // no error thrown
			} catch (Throwable t) {
				return expectedErrorClass.isInstance(t);
			}
		}
		
	}
	
	public AsyncInterval emptyInterval(Interval parent, String name) {
		return parent.newAsyncChild(new EmptyTask(name));
	}
	
	public PointImpl impl(Point p) {
		return (PointImpl)p;
	}

	public void expect(ExpectedErrorTask task) {
		Assert.assertTrue(Intervals.inline(task));		
	}

}
