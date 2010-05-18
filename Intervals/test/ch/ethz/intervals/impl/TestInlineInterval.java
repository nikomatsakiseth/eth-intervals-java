package ch.ethz.intervals.impl;

import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Assert;
import org.junit.Test;

import ch.ethz.intervals.InlineInterval;
import ch.ethz.intervals.Interval;
import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.RethrownException;
import ch.ethz.intervals.IntervalException.AlreadyExecuted;
import ch.ethz.intervals.task.AbstractTask;

public class TestInlineInterval extends Util {
	
	@Test public void separatedCreationAndExecutionOfInlineInterval() {
		final AtomicInteger i = new AtomicInteger(0);
		Intervals.inline(new AbstractTask("outer") {
			public void run(Interval subinterval) {
				InlineInterval inter = subinterval.newInlineChild(new AbstractTask("inner") {
					@Override public void run(Interval current) throws Exception {
						i.incrementAndGet();
					}
				});
				
				inter.execute();
			}
		});
		Assert.assertEquals(1, i.get());
	}

	@Test public void inlineIntervalExecutedTwice() {
		final AtomicInteger i = new AtomicInteger(0);
		
		try {
			Intervals.inline(new AbstractTask("outer") {
				public void run(Interval subinterval) {
					InlineInterval inter = subinterval.newInlineChild(new AbstractTask("inner") {
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
			assertThrew(err, IntervalException.AlreadyExecuted.class);
		}
		
		Assert.assertEquals(1, i.get());
	}
	
	@Test public void inlineIntervalNeverExecuted() {
		final AtomicInteger i = new AtomicInteger(0);
		
		try {
			Intervals.inline(new AbstractTask("outer") {
				public void run(Interval subinterval) {
					subinterval.newInlineChild(new AbstractTask("inner") {
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
					subinterval.newInlineChild(new AbstractTask("inner1") {
						@Override public void run(Interval current) throws Exception {
							i.addAndGet(1);
						}
					});
					
					subinterval.newInlineChild(new AbstractTask("inner2") {
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
					final InlineInterval inter = subinterval.newInlineChild(new AbstractTask("inner") {
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
					IntervalException.InlineIntervalNeverExecuted.class, 
					IntervalException.NotExecutedFromParent.class);
		}
		
		Assert.assertEquals(0, i.get());
	}
	
}
