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
	
	@Test public void testMBBB() {
		Intervals.inline(new AbstractTask("a") {			
			@Override public String toString() { return "a"; }
			@Override public void run(Interval root) {
				Interval a = root.newAsyncChild(new EmptyTask("a")); 
				Interval a1 = a.newAsyncChild(new EmptyTask("a1")); 
				Interval a12 = a1.newAsyncChild(new EmptyTask("a12")); 
				Interval a2 = a.newAsyncChild(new EmptyTask("a2")); 
				Interval b = root.newAsyncChild(new EmptyTask("b"));
				
				// Permitted because it is a duplicate:
				addIllegalEdge(a12.getEnd(), a1.getEnd(), IntervalException.MustBeBoundedBy.class);
				
				addLegalEdge(a1.getEnd(), a2.getStart());
				addLegalEdge(a1.getEnd(), a2.getEnd());
				
				addLegalEdge(b.getStart(), a2.getEnd());
				addLegalEdge(b.getStart(), a12.getEnd());
				addLegalEdge(b.getStart(), a1.getEnd());
				
				addLegalEdge(b.getEnd(), a2.getEnd());
				addLegalEdge(b.getEnd(), a12.getEnd());
				addLegalEdge(b.getEnd(), a1.getEnd());
				
				addIllegalEdge(a12.getStart(), a2.getEnd(), IntervalException.MustBeBoundedBy.class);				
				addIllegalEdge(a12.getEnd(), a2.getEnd(), IntervalException.MustBeBoundedBy.class);				
				addIllegalEdge(a12.getStart(), a2.getStart(), IntervalException.MustBeBoundedBy.class);				
				addIllegalEdge(a12.getEnd(), a2.getStart(), IntervalException.MustBeBoundedBy.class);			
				
				addIllegalEdge(a12.getStart(), b.getStart(), IntervalException.MustBeBoundedBy.class);
				addIllegalEdge(a12.getEnd(), b.getStart(), IntervalException.MustBeBoundedBy.class);
				addIllegalEdge(a12.getStart(), b.getEnd(), IntervalException.MustBeBoundedBy.class);
				addIllegalEdge(a12.getEnd(), b.getEnd(), IntervalException.MustBeBoundedBy.class);
			}

		});
	}
	
	@Test public void testMBBBWithSub() {
		Intervals.inline(new AbstractTask("root") {			
			@Override public void run(final Interval root) {
				Intervals.inline(new AbstractTask("a") {
					@Override public void run(final Interval a) {
						class Helper {
							Interval a12, a2, b;
						}
						final Helper h = new Helper();
						
						Intervals.inline(new AbstractTask("a1") {
							@Override public void run(Interval a1) {
								Intervals.inline(new AbstractTask("a12") {							
									@Override public void run(Interval a12) {
										h.a12 = a12;
									}
								});
							}
						});				
						h.a2 = a.newAsyncChild(new EmptyTask("a2"));
						h.b = root.newAsyncChild(new EmptyTask("b"));
		
						addIllegalEdge(h.a12.getStart(), h.a2.getEnd(), IntervalException.MustBeBoundedBy.class);				
						addIllegalEdge(h.a12.getEnd(), h.a2.getEnd(), IntervalException.MustBeBoundedBy.class);				
						addIllegalEdge(h.a12.getStart(), h.a2.getStart(), IntervalException.MustBeBoundedBy.class);				
						addIllegalEdge(h.a12.getEnd(), h.a2.getStart(), IntervalException.MustBeBoundedBy.class);				
						
						addIllegalEdge(h.a12.getStart(), h.b.getStart(), IntervalException.MustBeBoundedBy.class);
						addIllegalEdge(h.a12.getEnd(), h.b.getStart(), IntervalException.MustBeBoundedBy.class);
						addIllegalEdge(h.a12.getStart(), h.b.getEnd(), IntervalException.MustBeBoundedBy.class);
						addIllegalEdge(h.a12.getEnd(), h.b.getEnd(), IntervalException.MustBeBoundedBy.class);
					}
				});
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
