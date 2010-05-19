package ch.ethz.intervals.impl;

import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Assert;

import ch.ethz.intervals.AsyncInterval;
import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.Point;
import ch.ethz.intervals.RethrownException;
import ch.ethz.intervals.task.AbstractTask;
import ch.ethz.intervals.task.EmptyTask;
import ch.ethz.intervals.task.ResultTask;

public class TestUtil {
	
	public void assertThrew(
			RethrownException err,
			Class<?>... classes) 
	{
		Assert.assertEquals(classes.length, err.allErrors().size());
		
		boolean[] matched = new boolean[classes.length];
		for(Throwable thr : err.allErrors()) {
			int i;
			
			for(i = 0; i < classes.length; i++) {
				if(!matched[i] && classes[i].isInstance(thr)) {
					matched[i] = true;
					break;
				}
			}
			
			if(i == classes.length) {
				Assert.fail(String.format(
						"Unexpected error %s: %s", thr.getClass().getName(), thr));
			}
		}
		for(int i = 0; i < matched.length; i++) {
			Assert.assertTrue(
					String.format("Class %s was not thrown", classes[i]),
					matched[i]);
		}
	}

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
	
	@SuppressWarnings("serial")
	static class TestException extends RuntimeException { 
	}

	public static class IncTask extends AbstractTask {
		public final AtomicInteger i;
		public final int amnt;
	
		public IncTask(String name, AtomicInteger i) {
			this(name, i, 1);
		}
	
		public IncTask(String name, AtomicInteger i, int amnt) {
			super(name);
			this.i = i;
			this.amnt = amnt;
		}
	
		@Override
		public void run(Interval current) {
			i.addAndGet(amnt);
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
