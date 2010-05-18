package ch.ethz.intervals.impl;

import java.util.Arrays;

import org.junit.Assert;
import org.junit.Test;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.Point;
import ch.ethz.intervals.task.AbstractTask;

public class TestHB extends Util {
	
	private void assertHb(Point from, Point to) {
		Assert.assertTrue(from.hb(to));
		Assert.assertFalse(to.hb(from));
	}	
	
	private void assertUnordered(Point pnt1, Point pnt2) {
		Assert.assertFalse(pnt1.hb(pnt2));
		Assert.assertFalse(pnt2.hb(pnt1));
	}	
	
	@Test public void ignoresSpeculative() {
		Intervals.inline(new AbstractTask() {			
			@Override public void run(Interval subinterval) {
				Interval a = emptyInterval(subinterval, "a");
				Interval b = emptyInterval(subinterval, "b");
				impl(a.getEnd()).optimisticallyAddEdge(impl(b.getStart()));
				assertUnordered(a.getEnd(), b.getStart());
				impl(a.getEnd()).recoverFromCycle(impl(b.getStart()));
			}
		});
	}
	
	@Test public void trans() {
		class Helper {
			Interval a, b, c;
		}
		final Helper h = new Helper();
		
		Intervals.inline(new AbstractTask() {			
			@Override public void run(Interval subinterval) {
				h.a = emptyInterval(subinterval, "a");
				h.b = emptyInterval(subinterval, "b");
				h.c = emptyInterval(subinterval, "c");
				
				Intervals.addHb(h.a.getEnd(), h.b.getStart());
				Intervals.addHb(h.b.getEnd(), h.c.getStart());
			}
		});
		
		assertHb(h.a.getStart(), h.c.getEnd());
	}	
	
	@Test public void parChild() {
		class Helper {
			Interval a, a1;
		}
		final Helper h = new Helper();
		
		Intervals.inline(new AbstractTask() {			
			@Override public void run(Interval subinterval) {
				h.a = emptyInterval(subinterval, "a");
				h.a1 = emptyInterval(h.a, "a1");
			}
		});
		
		assertHb(h.a.getStart(), h.a1.getStart());
		assertHb(h.a1.getEnd(), h.a.getEnd());
	}	
	
	@Test public void bounds() {
		class Helper {
			Interval a, a1, b, b1;
		}
		final Helper h = new Helper();
		
		Intervals.inline(new AbstractTask() {			
			@Override public void run(Interval subinterval) {
				h.a = emptyInterval(subinterval, "a");
				h.a1 = emptyInterval(h.a, "a1");
				h.b = emptyInterval(subinterval, "b");
				h.b1 = emptyInterval(h.b, "b1");
				
				Intervals.addHb(h.a.getEnd(), h.b.getStart());
			}
		});
		
		assertHb(h.a1.getStart(), h.b.getStart());
		assertHb(h.a1.getStart(), h.b1.getEnd());
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
		
		Intervals.inline(new AbstractTask() {			
			@Override public String toString() { return "p"; }
			@Override public void run(Interval p) {
				h.p = p;
				
				Intervals.inline(new AbstractTask() {					
					@Override public String toString() { return "a0"; }
					@Override public void run(Interval i) {
						h.a[0] = i;
						
						Intervals.inline(new AbstractTask() {					
							@Override public String toString() { return "a1"; }
							@Override public void run(Interval i) {
								h.a[1] = i;
								h.a[2] = emptyInterval(i, "a2");
							}
						});
					}
				});
				
				Intervals.inline(new AbstractTask() {					
					@Override public String toString() { return "b0"; }
					@Override public void run(Interval i) {
						h.b[0] = i;
						
						Intervals.inline(new AbstractTask() {					
							@Override public String toString() { return "b1"; }
							@Override public void run(Interval i) {
								h.b[1] = i;
								h.b[2] = emptyInterval(i, "b2");
							}
						});
					}
				});
			}
		});
		
		for(int i = 0; i <= 2; i++) {
			assertHb(h.p.getStart(), h.a[i].getStart());
			assertHb(h.p.getStart(), h.a[i].getEnd());
			assertHb(h.a[i].getStart(), h.p.getEnd());
		}
		
		for(Interval[] arr : Arrays.asList(h.a, h.b)) {
			for(int i = 0; i <= 2; i++) {
				for(int j = i+1; j <= 2; j++) {
					assertHb(arr[i].getStart(), arr[j].getStart());
					assertHb(arr[i].getStart(), arr[j].getEnd());
					assertHb(arr[j].getStart(), arr[i].getEnd());				
				}
			}
		}
		
		for(int i = 0; i <= 2; i++)
			for(int j = 0; j <= 2; j++)
				assertHb(h.a[i].getStart(), h.b[j].getStart());
	}

}
