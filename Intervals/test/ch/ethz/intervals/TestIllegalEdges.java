package ch.ethz.intervals;

import org.junit.Assert;
import org.junit.Test;

public class TestIllegalEdges {

	private void addIllegalEdge(Point from, Point to, Class<? extends IntervalException> err) {
		try {
			Intervals.addHb(from, to);
			Assert.fail("No error");
		} catch (IntervalException e) {
			Assert.assertTrue("Wrong class: "+e, err.isInstance(e));
		}
	}
	
	private void addLegalEdge(Point from, Point to) {
		Intervals.addHb(from, to);
	}
	
	@Test public void testCE() {
		Intervals.subinterval("a", new VoidSubinterval() {			
			@Override public void run(Interval a) {
				Interval a1 = new EmptyInterval(a, "a1"); 
				Interval a12 = new EmptyInterval(a1, "a12"); 
				addIllegalEdge(a12.end, a1.start, CycleException.class);
			}
		});
	}
	
	@Test public void testMBBB() {
		Intervals.subinterval("a", new VoidSubinterval() {			
			@Override public void run(Interval a) {
				Interval a1 = new EmptyInterval(a, "a1"); 
				Interval a12 = new EmptyInterval(a1, "a12"); 
				Interval a2 = new EmptyInterval(a, "a2"); 
				Interval b = new EmptyInterval(Intervals.root(), "a");

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
		Intervals.subinterval("a", new VoidSubinterval() {			
			@Override public void run(Interval a) {
				class Helper {
					Interval a1, a12, a2, b;
				}
				final Helper h = new Helper();
				
				Intervals.subinterval("a1", new VoidSubinterval() {					
					@Override public void run(Interval a1) {
						h.a1 = a1;
						Intervals.subinterval("a12", new VoidSubinterval() {							
							@Override public void run(Interval a12) {
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

}
