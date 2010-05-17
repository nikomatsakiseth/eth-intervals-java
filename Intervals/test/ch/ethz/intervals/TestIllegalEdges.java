package ch.ethz.intervals;

import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Assert;
import org.junit.Test;

import ch.ethz.intervals.impl.IntervalImpl;
import ch.ethz.intervals.impl.PointImpl;

public class TestIllegalEdges {

	private void addIllegalEdge(PointImpl from, PointImpl to, Class<? extends IntervalException> err) {
		try {
			Intervals.addHb(from, to);
			Assert.fail("No error");
		} catch (IntervalException e) {
			Assert.assertTrue("Wrong class: "+e, err.isInstance(e));
		}
	}
	
	private void addLegalEdge(PointImpl from, PointImpl to) {
		Intervals.addHb(from, to);
	}
	
	@Test public void testCE() {
		Intervals.inline(new VoidInlineTask() {			
			@Override public String toString() { return "a"; }
			@Override public void run(IntervalImpl a) {
				IntervalImpl a1 = new EmptyInterval(a, "a1"); 
				IntervalImpl a12 = new EmptyInterval(a1, "a12"); 
				addIllegalEdge(a12.end, a1.start, CycleException.class);
			}
		});
	}
	
	@Test public void testMBBB() {
		Intervals.inline(new VoidInlineTask() {			
			@Override public String toString() { return "a"; }
			@Override public void run(IntervalImpl a) {
				IntervalImpl a1 = new EmptyInterval(a, "a1"); 
				IntervalImpl a12 = new EmptyInterval(a1, "a12"); 
				IntervalImpl a2 = new EmptyInterval(a, "a2"); 
				IntervalImpl b = new EmptyInterval(Intervals.root(), "a");
				
				// Permitted because it is a duplicate:
				addIllegalEdge(a12.end, a1.end, MustBeBoundedByException.class);
				
				addLegalEdge(a1.end, a2.start);
				addLegalEdge(a1.end, a2.end);
				
				addLegalEdge(b.start, a2.end);
				addLegalEdge(b.start, a12.end);
				addLegalEdge(b.start, a1.end);
				
				addLegalEdge(b.end, a2.end);
				addLegalEdge(b.end, a12.end);
				addLegalEdge(b.end, a1.end);
				
				addIllegalEdge(a12.start, a2.end, MustBeBoundedByException.class);				
				addIllegalEdge(a12.end, a2.end, MustBeBoundedByException.class);				
				addIllegalEdge(a12.start, a2.start, MustBeBoundedByException.class);				
				addIllegalEdge(a12.end, a2.start, MustBeBoundedByException.class);			
				
				addIllegalEdge(a12.start, b.start, MustBeBoundedByException.class);
				addIllegalEdge(a12.end, b.start, MustBeBoundedByException.class);
				addIllegalEdge(a12.start, b.end, MustBeBoundedByException.class);
				addIllegalEdge(a12.end, b.end, MustBeBoundedByException.class);
			}

		});
	}
	
	@Test public void testMBBBWithSub() {
		Intervals.inline(new VoidInlineTask() {			
			@Override public String toString() { return "a"; }
			@Override public void run(IntervalImpl a) {
				class Helper {
					IntervalImpl a1, a12, a2, b;
				}
				final Helper h = new Helper();
				
				Intervals.inline(new VoidInlineTask() {					
					@Override public String toString() { return "a1"; }
					@Override public void run(IntervalImpl a1) {
						h.a1 = a1;
						Intervals.inline(new VoidInlineTask() {							
							@Override public String toString() { return "a12"; }
							@Override public void run(IntervalImpl a12) {
								h.a12 = a12;
							}
						});
					}
				});				
				h.a2 = new EmptyInterval(a, "a2"); 
				h.b = new EmptyInterval(Intervals.root(), "a");

				addIllegalEdge(h.a12.start, h.a2.end, MustBeBoundedByException.class);				
				addIllegalEdge(h.a12.end, h.a2.end, MustBeBoundedByException.class);				
				addIllegalEdge(h.a12.start, h.a2.start, MustBeBoundedByException.class);				
				addIllegalEdge(h.a12.end, h.a2.start, MustBeBoundedByException.class);				
				
				addIllegalEdge(h.a12.start, h.b.start, MustBeBoundedByException.class);
				addIllegalEdge(h.a12.end, h.b.start, MustBeBoundedByException.class);
				addIllegalEdge(h.a12.start, h.b.end, MustBeBoundedByException.class);
				addIllegalEdge(h.a12.end, h.b.end, MustBeBoundedByException.class);
			}

		});
	}

	@Test public void testIllegalToCreateChildEvenWithEdgeToEnd() {
		final AtomicInteger integer = new AtomicInteger();
		try {
			Intervals.inline(new VoidInlineTask() {			
				@Override public void run(IntervalImpl subinterval) {
					final IntervalImpl a = new TestInterval.IncTask(subinterval, "a", integer);
					
					new IntervalImpl(subinterval, "b") {
						@Override protected void run() {
							new TestInterval.IncTask(a, "a1", integer, 10);
						}
					};
				}
			});
			Assert.fail();
		} catch (RethrownException e) {
			Assert.assertTrue(e.getCause() instanceof EdgeNeededException);
		}
		Assert.assertEquals(1, integer.get()); // a should execute
	}
	
}
