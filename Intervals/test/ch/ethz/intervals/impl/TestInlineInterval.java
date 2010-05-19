package ch.ethz.intervals.impl;

import static ch.ethz.intervals.Intervals.context;

import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Assert;
import org.junit.Test;

import ch.ethz.intervals.InlineInterval;
import ch.ethz.intervals.Interval;
import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.RethrownException;
import ch.ethz.intervals.task.AbstractTask;

public class TestInlineInterval extends TestUtil {
	
	@Test public void separatedCreationAndExecutionOfInlineInterval() {
		final AtomicInteger i = new AtomicInteger(0);
		Intervals.inline(new AbstractTask("outer") {
			public void run(Interval subinterval) {
				InlineInterval inter = context().unexecutedInline(new AbstractTask("inner") {
					@Override public void run(Interval current) throws Exception {
						i.incrementAndGet();
					}
				});
				
				inter.execute();
			}
		});
		Assert.assertEquals(1, i.get());
	}
	
	@Test public void errorInAttachedTo() {
		final AtomicInteger i = new AtomicInteger(0);
		try {
			Intervals.inline(new AbstractTask("outer") {
				public void run(Interval subinterval) {
					InlineInterval inter = context().unexecutedInline(new AbstractTask("inner") {
						@Override public void attachedTo(Interval current) {
							throw new TestException();
						}
						
						@Override public void run(Interval current) {
							i.incrementAndGet();
						}
					});
					
					inter.execute();
				}
			});
		} catch (RethrownException err) {
			assertThrew(err, TestException.class);
		}
	
		Assert.assertEquals(0, i.get());
	}

	@Test public void errorCaughtInAttachedTo() {
		final AtomicInteger i = new AtomicInteger(0);
		Intervals.inline(new AbstractTask("outer") {
			public void run(Interval subinterval) {
				try {
					Intervals.inline(new AbstractTask("inner") {
						@Override public void attachedTo(Interval current) {
							throw new TestException();
						}
						
						@Override public void run(Interval current) {
							i.incrementAndGet();
						}
					});
				} catch (TestException e) {
					// By catching the error, we allow outer to complete
					// normally.  At some point, this caused an error because
					// inner was never executed even though outer completed
					// normally.  But since inner never completed construction,
					// it could not have been executed.
				}
			}
		});
		Assert.assertEquals(0, i.get());
	}

	@Test public void inlineIntervalExecutedTwice() {
		final AtomicInteger i = new AtomicInteger(0);
		
		try {
			Intervals.inline(new AbstractTask("outer") {
				public void run(Interval subinterval) {
					InlineInterval inter = context().unexecutedInline(new AbstractTask("inner") {
						@Override public void run(Interval current) throws Exception {
							i.incrementAndGet();
						}
					});
					
					inter.execute();
					inter.execute();
				}
			});
			Assert.fail("No exception thrown.");
		} catch (RethrownException err) {
			assertThrew(err, IntervalException.AlreadyScheduled.class);
		}
		
		Assert.assertEquals(1, i.get());
	}
	
	@Test public void inlineIntervalNeverExecuted() {
		final AtomicInteger i = new AtomicInteger(0);
		
		try {
			Intervals.inline(new AbstractTask("outer") {
				public void run(Interval subinterval) {
					context().unexecutedInline(new AbstractTask("inner") {
						@Override public void run(Interval current) throws Exception {
							i.incrementAndGet();
						}
					});
				}
			});
			Assert.fail("No exception thrown.");
		} catch (RethrownException err) {
			assertThrew(err, 
					IntervalException.InlineIntervalNeverExecuted.class);
		}
		
		Assert.assertEquals(0, i.get());
	}
	
	@Test public void multipleInlineIntervalsNeverExecuted() {
		final AtomicInteger i = new AtomicInteger(0);
		
		try {
			Intervals.inline(new AbstractTask("outer") {
				public void run(Interval subinterval) {
					context().unexecutedInline(new AbstractTask("inner1") {
						@Override public void run(Interval current) throws Exception {
							i.addAndGet(1);
						}
					});
					
					context().unexecutedInline(new AbstractTask("inner2") {
						@Override public void run(Interval current) throws Exception {
							i.addAndGet(10);
						}
					});
				}
			});
			Assert.fail("No exception thrown.");
		} catch (RethrownException err) {
			assertThrew(err, 
					IntervalException.InlineIntervalNeverExecuted.class,
					IntervalException.InlineIntervalNeverExecuted.class);
		}
		
		Assert.assertEquals(0, i.get());
	}
	
	@Test public void inlineIntervalExecutedByWrongParent() {
		final AtomicInteger i = new AtomicInteger(0);
		
		try {
			Intervals.inline(new AbstractTask("outer") {
				public void run(Interval subinterval) {
					final InlineInterval inter = context().unexecutedInline(new AbstractTask("inner") {
						@Override public void run(Interval current) throws Exception {
							i.incrementAndGet();
						}
					});
					
					Intervals.inline(
							new AbstractTask("async") {
								@Override public void run(Interval current) {
									inter.execute();
								}
							}
					);
				}
			});
			Assert.fail("No exception thrown.");
		} catch (RethrownException err) {
			assertThrew(err, 
					IntervalException.AlreadyScheduled.class);
		}
		
		Assert.assertEquals(0, i.get());
	}

}
