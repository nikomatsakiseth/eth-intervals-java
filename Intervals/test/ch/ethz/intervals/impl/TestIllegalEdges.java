package ch.ethz.intervals.impl;

import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Assert;
import org.junit.Test;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.Point;
import ch.ethz.intervals.RethrownException;
import ch.ethz.intervals.task.AbstractTask;
import ch.ethz.intervals.task.EmptyTask;

public class TestIllegalEdges {

	private void addIllegalEdge(Point from, Point to, Class<? extends IntervalException> err) {
		try {
			Intervals.addHb(from, to);
			Assert.fail("No error");
		} catch (IntervalException e) {
			Assert.assertEquals("Wrong class", err, e.getClass());
		}
	}
	
	private void addLegalEdge(Point from, Point to) {
		Intervals.addHb(from, to);
	}
	
	@Test public void testCE() {
		Intervals.inline(new AbstractTask("a") {			
			@Override public String toString() { return "a"; }
			@Override public void run(Interval a) {
				Interval a1 = a.newAsyncChild(new EmptyTask("a1"));
				Interval a12 = a1.newAsyncChild(new EmptyTask("a1")); 
				addIllegalEdge(a12.getEnd(), a1.getStart(), IntervalException.Cycle.class);
			}
		});
	}
	
	@Test public void testIllegalToCreateChildEvenWithEdgeToEnd() {
		final AtomicInteger integer = new AtomicInteger();
		try {
			Intervals.inline(new AbstractTask("a") {			
				@Override public void run(Interval subinterval) {
					final Interval a = subinterval.newAsyncChild(new TestInterval.IncTask("a", integer));
					
					subinterval.newAsyncChild(new AbstractTask("b") {
						@Override public void run(Interval current) {
							a.newAsyncChild(new TestInterval.IncTask("a1", integer, 10));
						}						
					});
				}
			});
			Assert.fail();
		} catch (RethrownException e) {
			Assert.assertTrue(e.getCause() instanceof IntervalException.MustHappenBefore);
		}
		Assert.assertEquals(1, integer.get()); // a should execute
	}
	
}
