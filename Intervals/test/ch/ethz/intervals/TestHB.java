package ch.ethz.intervals;

import org.junit.Assert;
import org.junit.Test;

public class TestHB {
	
	@Test public void ignoresSpeculative() {
		Intervals.subinterval(new VoidSubinterval() {			
			@Override public void run(Interval subinterval) {
				Interval a = new EmptyInterval(subinterval, "a");
				Interval b = new EmptyInterval(subinterval, "b");
				Intervals.optimisticallyAddEdge(a.end, b.start);				
				Assert.assertFalse(a.end.hb(b.start));
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
		
		Assert.assertTrue(h.a.start.hb(h.c.end));
		Assert.assertFalse(h.c.end.hb(h.a.start));
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
		
		Assert.assertTrue(h.a.start.hb(h.a1.start));
		Assert.assertFalse(h.a1.start.hb(h.a.start));
		Assert.assertTrue(h.a1.end.hb(h.a.end));
		Assert.assertFalse(h.a.end.hb(h.a1.end));
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
		
		Assert.assertTrue(h.a1.start.hb(h.b.start));
		Assert.assertTrue(h.a1.start.hb(h.b1.end));
		Assert.assertTrue(h.a1.start.hb(h.b1.end));
	}	

	@Test public void subPattern() {
		// Create the following pattern:
		// 
		// 0.s--1.s--2.s--2.e--1.e--3.s--4.s--4.e--3.e--0.e
		//
		// which is kind of a stress test for our internal representation.
		class Helper {
			Interval[] i = new Interval[5];
		}
		final Helper h = new Helper();
		
		Intervals.subinterval(new VoidSubinterval() {			
			@Override public void run(Interval a) {
				h.i[0] = a;
				
				Intervals.subinterval(new VoidSubinterval() {					
					@Override public void run(Interval b) {
						h.i[1] = b;
						
						Intervals.subinterval(new VoidSubinterval() {					
							@Override public void run(Interval c) {
								h.i[2] = c;
							}
						});
					}
				});
				
				Intervals.subinterval(new VoidSubinterval() {					
					@Override public void run(Interval d) {
						h.i[3] = d;
						
						Intervals.subinterval(new VoidSubinterval() {					
							@Override public void run(Interval e) {
								h.i[4] = e;
							}
						});
					}
				});
			}
		});
		
		for(int j = 1; j <= 4; j++) {
			Assert.assertTrue(h.i[0].start.hb(h.i[j].start));
			Assert.assertTrue(h.i[0].start.hb(h.i[j].end));			
			Assert.assertFalse(h.i[0].end.hb(h.i[j].start));
		}
		
		Assert.assertTrue(h.i[1].start.hb(h.i[2].start));
		Assert.assertTrue(h.i[1].start.hb(h.i[2].end));
		Assert.assertFalse(h.i[1].end.hb(h.i[2].start));
		
		for(int j = 3; j <= 4; j++) {
			Assert.assertTrue(h.i[1].start.hb(h.i[j].start));
			Assert.assertTrue(h.i[1].start.hb(h.i[j].end));
			Assert.assertTrue(h.i[1].end.hb(h.i[j].start));
		}
		
		for(int j = 3; j <= 4; j++) {
			Assert.assertTrue(h.i[2].start.hb(h.i[j].start));
			Assert.assertTrue(h.i[2].start.hb(h.i[j].end));
			Assert.assertTrue(h.i[2].end.hb(h.i[j].start));
		}
	}	
	
}
