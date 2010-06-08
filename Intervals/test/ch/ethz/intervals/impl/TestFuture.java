package ch.ethz.intervals.impl;

import junit.framework.Assert;

import org.junit.Test;

import ch.ethz.intervals.AsyncInterval;
import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.task.AbstractTask;

public class TestFuture extends TestUtil {
	
	/**
	 * One way to implement a future using intervals:
	 * the {@link #inter} field contains a child interval
	 * which invokes {@link #compute()} and stores the
	 * returned value into {@link #result}.  
	 * The method {@link #get()} then "joins" {@link #inter}
	 * and returns {@link #result}.  
	 */
	abstract class IntervalFuture<R> {
		
		private final Interval inter;
		private R result;
		
		public IntervalFuture(Interval parent) {
			AsyncInterval inter = parent.newAsyncChild(new AbstractTask("future") {
				@Override
				public void run(Interval current) {
					result = compute();
				}
			});
			inter.schedule();
			this.inter = inter;
		}
		
		protected abstract R compute();
		
		public R get() {
			// Intervals.join() creates an inline subinterval X
			// where inter.end -> X.start.  Therefore, X will not
			// complete until inter has completed.  If this would
			// cause a cycle (for example, if compute() invoked get()),
			// then an exception would be thrown.
			Intervals.join(inter);
			return result;
		}
		
	}
	
	/** Computes the log of the input in a separate interval. */
	class ComputeLog extends IntervalFuture<Double> {
		
		private final double input;

		public ComputeLog(Interval parent, double input) {
			super(parent);
			this.input = input;
		}

		@Override
		protected Double compute() {
			return Math.log(input);
		}
		
	}
	
	@Test public void future() {
		
		Intervals.inline(new AbstractTask() {
			@Override
			public void run(Interval current) {
				
				// Create two parallel intervals computing the
				// log of 1024 and 2048, respectively.
				final ComputeLog tenTwentyFour = new ComputeLog(current, 1024);
				final ComputeLog twoThousandFortyEight = new ComputeLog(current, 2048);
				
				// Create a third asynchronous task.  This will force
				// the two futures in sequences.
				current.newAsyncChild(new AbstractTask("sibling") {
					@Override
					public void run(Interval current) throws Exception {
						Assert.assertEquals(Math.log(1024), tenTwentyFour.get());
						Assert.assertEquals(Math.log(2048), twoThousandFortyEight.get());
					}
				});
				
			}
		});
		
	}

}
