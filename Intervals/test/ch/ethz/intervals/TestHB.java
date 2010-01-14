package ch.ethz.intervals;

import java.util.Arrays;

import org.junit.Assert;
import org.junit.Test;

public class TestHB {
	
	private void assertHb(Point from, Point to) {
		Assert.assertTrue(from.hb(to));
		Assert.assertFalse(to.hb(from));
	}	
	
	private void assertUnordered(Point pnt1, Point pnt2) {
		Assert.assertFalse(pnt1.hb(pnt2));
		Assert.assertFalse(pnt2.hb(pnt1));
	}	
	
	@Test public void ignoresSpeculative() {
		Intervals.subinterval(new VoidSubinterval() {			
			@Override public void run(Interval subinterval) {
				Interval a = new EmptyInterval(subinterval, "a");
				Interval b = new EmptyInterval(subinterval, "b");
				Intervals.optimisticallyAddEdge(a.end, b.start);
				assertUnordered(a.end, b.start);
				Intervals.recoverFromCycle(a.end, b.start);
			}
		});
	}
	
	@Test public void trans() {
		class Helper {
			Interval a, b, c;
		}
		final Helper h = new Helper();
		
		Intervals.subinterval(new VoidSubinterval() {			
			@Override public void run(Interval subinterval) {
				h.a = new EmptyInterval(subinterval, "a");
				h.b = new EmptyInterval(subinterval, "b");
				h.c = new EmptyInterval(subinterval, "c");
				
				Intervals.addHb(h.a.end, h.b.start);
				Intervals.addHb(h.b.end, h.c.start);
			}
		});
		
		assertHb(h.a.start, h.c.end);
	}	
	
	@Test public void parChild() {
		class Helper {
			Interval a, a1;
		}
		final Helper h = new Helper();
		
		Intervals.subinterval(new VoidSubinterval() {			
			@Override public void run(Interval subinterval) {
				h.a = new EmptyInterval(subinterval, "a");
				h.a1 = new EmptyInterval(h.a, "a1");
			}
		});
		
		assertHb(h.a.start, h.a1.start);
		assertHb(h.a1.end, h.a.end);
	}	
	
	@Test public void bounds() {
		class Helper {
			Interval a, a1, b, b1;
		}
		final Helper h = new Helper();
		
		Intervals.subinterval(new VoidSubinterval() {			
			@Override public void run(Interval subinterval) {
				h.a = new EmptyInterval(subinterval, "a");
				h.a1 = new EmptyInterval(h.a, "a1");
				h.b = new EmptyInterval(subinterval, "b");
				h.b1 = new EmptyInterval(h.b, "b1");
				
				Intervals.addHb(h.a.end, h.b.start);
			}
		});
		
		assertHb(h.a1.start, h.b.start);
		assertHb(h.a1.start, h.b1.end);
	}	

	@Test public void complexPattern() {
		// Create the following pattern:
		// 
		// p.s                                                                              p.e
		//    \-a0.s                            a0.e--b0.s                            b0.e-/
		//          \-a1.s-|..............a1.e-/          \-b1.s-|..............b1.e-/
		//                  \-a2.s--a2.e-/                        \-b2.s--b2.e-/
		//
		// which is kind of a stress test for our internal representation.
		class Helper {
			Interval p;
			Interval[] a = new Interval[3];
			Interval[] b = new Interval[3];
		}
		final Helper h = new Helper();
		
		Intervals.subinterval("p", new VoidSubinterval() {			
			@Override public void run(Interval p) {
				h.p = p;
				
				Intervals.subinterval("a0", new VoidSubinterval() {					
					@Override public void run(Interval i) {
						h.a[0] = i;
						
						Intervals.subinterval("a1", new VoidSubinterval() {					
							@Override public void run(Interval i) {
								h.a[1] = i;
								h.a[2] = new EmptyInterval(i, "a2");
							}
						});
					}
				});
				
				Intervals.subinterval("b0", new VoidSubinterval() {					
					@Override public void run(Interval i) {
						h.b[0] = i;
						
						Intervals.subinterval("b1", new VoidSubinterval() {					
							@Override public void run(Interval i) {
								h.b[1] = i;
								h.b[2] = new EmptyInterval(i, "b2");
							}
						});
					}
				});
			}
		});
		
		for(int i = 0; i <= 2; i++) {
			assertHb(h.p.start, h.a[i].start);
			assertHb(h.p.start, h.a[i].end);
			assertHb(h.a[i].start, h.p.end);
		}
		
		for(Interval[] arr : Arrays.asList(h.a, h.b)) {
			for(int i = 0; i <= 2; i++) {
				for(int j = i+1; j <= 2; j++) {
					assertHb(arr[i].start, arr[j].start);
					assertHb(arr[i].start, arr[j].end);
					assertHb(arr[j].start, arr[i].end);				
				}
			}
		}
		
		for(int i = 0; i <= 2; i++)
			for(int j = 0; j <= 2; j++)
				assertHb(h.a[i].start, h.b[j].start);
	}

}
