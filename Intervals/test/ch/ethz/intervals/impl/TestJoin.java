package ch.ethz.intervals.impl;

import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Assert;
import org.junit.Test;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.IntervalException.Cycle;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.RethrownException;
import ch.ethz.intervals.task.AbstractTask;

public class TestJoin extends TestUtil {

	@Test public void acyclicJoin() {
		final AtomicInteger stamp = new AtomicInteger(0);
		
		final int[] stamps = new int[2];
		
		Intervals.inline(new AbstractTask("outer") {
			@Override public void run(Interval outer) {
				final Interval[] intervals = new Interval[2];
			
				intervals[0] = outer.newAsyncChild(new AbstractTask("0") {
					@Override public void run(Interval _) {
						Intervals.join(intervals[1]);
						stamps[0] = stamp.incrementAndGet();
					}
				});
				
				intervals[1] = outer.newAsyncChild(new AbstractTask("1") {
					@Override public void run(Interval _) {
						stamps[1] = stamp.incrementAndGet();
					}
				});
			}
		});
		
		Assert.assertEquals(2, stamps[0]);
		Assert.assertEquals(1, stamps[1]);
	}

	@Test public void inlineIntervalUsedToCreateCyclicJoin() {
		final AtomicInteger succ = new AtomicInteger(0);
		final AtomicInteger fail = new AtomicInteger(0);
		
		Intervals.inline(new AbstractTask("outer") {
			@Override public void run(Interval outer) {
				final Interval[] intervals = new Interval[2];
				
				intervals[0] = outer.newAsyncChild(new AbstractTask("0") {
					@Override public void run(Interval _) {
						try {
							Intervals.join(intervals[1]);
							succ.addAndGet(1);
						} catch (IntervalException.Cycle c) {
							fail.addAndGet(1);
						}
					}
				});
				
				intervals[1] = outer.newAsyncChild(new AbstractTask("1") {
					@Override public void run(Interval _) {
						try {
							Intervals.join(intervals[0]);
							succ.addAndGet(1);
						} catch (IntervalException.Cycle c) {
							fail.addAndGet(1);
						}
					}
				});
			}
		});
		
		Assert.assertTrue("At most one should succeed", succ.get() <= 1);
		Assert.assertEquals(2, succ.get() + fail.get());
	}

	@Test public void inlineIntervalUsedToCreateCyclicJoinCatchFromOutside() {
		final AtomicInteger succ = new AtomicInteger(0);
		
		try {
			Intervals.inline(new AbstractTask("outer") {
				@Override public void run(Interval outer) {
					final Interval[] intervals = new Interval[2];
					
					intervals[0] = outer.newAsyncChild(new AbstractTask("0") {
						@Override public void run(Interval _) {
							Intervals.join(intervals[1]);
							succ.addAndGet(1);
						}
					});
					
					intervals[1] = outer.newAsyncChild(new AbstractTask("1") {
						@Override public void run(Interval _) {
							Intervals.join(intervals[0]);
							succ.addAndGet(1);
						}
					});
				}
			});
			Assert.fail("No error");
		} catch (RethrownException e) {
			if(succ.get() == 0) {
				assertThrew(e, IntervalException.Cycle.class, IntervalException.Cycle.class);
			} else if (succ.get() == 1) {
				assertThrew(e, IntervalException.Cycle.class);
			} else {
				Assert.fail("Both cannot succeed");
			}
		}
	}

	@Test 
	public void joinThrowsCyclicOnCycleAndNotRethrown() {
		final AtomicInteger succ = new AtomicInteger(0);
		
		Intervals.inline(new AbstractTask("outer") {
				@Override
				public void run(Interval outer) throws Exception {
					try {
						Intervals.join(outer);
					} catch (Cycle c) {
						succ.addAndGet(1);
					}
				}
		});
		
		Assert.assertEquals(1, succ.get());
	}

	@Test 
	public void joinRethrowsErrors() {
		final AtomicInteger succ = new AtomicInteger(0);
		
		Intervals.inline(new AbstractTask("outer") {
				@Override
				public void run(Interval outer) throws Exception {
					final Interval[] intervals = new Interval[3];
					
					intervals[0] = outer.newAsyncChild(new AbstractTask("catcher") {
						@Override public void run(Interval current) {}
						@Override
						public Set<? extends Throwable> catchErrors(Set<Throwable> errors) {
							return null;
						}
					});
					
					intervals[1] = intervals[0].newAsyncChild(new ThrowTask());
					
					intervals[2] = outer.newAsyncChild(new AbstractTask("joiner") {
						@Override
						public void run(Interval current) throws Exception {
							try {
								// At this point, intervals[1] will always have
								// occurred.  However, no errors will have prop
								// to intervals[2] because intervals[0] will have
								// caught them.  This join however will cause
								// the errors to be rethrown.
								Intervals.join(intervals[1]);
							} catch (RethrownException e) {
								assertThrew(e, TestException.class);
								succ.addAndGet(1);
							}
						}
					});
					
					Intervals.addHb(intervals[0], intervals[2]);
				}
		});
		
		Assert.assertEquals(1, succ.get());
	}
	
}
