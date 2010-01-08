package ch.ethz.intervals;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

public class TestDynamicGuard {
	
	/**
	 * The guard goes from being wr owned in {@code a1} to rd shared 
	 * in {@code a2} and its children, and in the {@code a3}
	 * interval should transition to rd owner and finally wr owned
	 * again.
	 */
	@Test public void testRdOwnedAllowsWr() {
		
		// |-a-----------------------------|
		//   |-a1-| -> |-a2----| -> |-a3-|
		//              |-a21-|
		//              |-a22-|

		final int a2children = 2;
		final List<Boolean> results = new ArrayList<Boolean>();
		
		Intervals.subinterval(new VoidSubinterval() { 
			@Override public void run(Interval a) {
				final DynamicGuard dg = new DynamicGuard();
				
				final Interval a1 = new Interval(a) {
					@Override public String toString() { return "a1"; }
					@Override protected void run() {
						results.add(dg.isReadable());
						results.add(dg.isWritable());
						results.add(dg.isReadable());
					}
				};
				
				final Interval a2 = new Interval(a) {					
					{ Intervals.addHb(a1.end, this.start); }
					@Override public String toString() { return "a2"; }
					@Override protected void run() {
						results.add(dg.isReadable());
						
						for(int i = 1; i <= a2children; i++) {
							final int num = i;
							new Interval(this) {
								@Override public String toString() { return "a2"+num; }
								@Override protected void run() {
									results.add(dg.isReadable());
								}
							};
						}
					}
				};
				
				new Interval(a) {										
					{ Intervals.addHb(a2.end, this.start); }
					@Override public String toString() { return "a3"; }					
					@Override protected void run() {
						results.add(dg.isReadable());
						results.add(dg.isReadable());
						results.add(dg.isWritable());
					}					
				};
			}
		});
		
		assertEquals(3+1+a2children+3, results.size());
		for(int i = 0; i < results.size(); i++)
			assertEquals(String.format("index %d", i), true, results.get(i));
	}	
	
	/**
	 * Similar to {@link #testRdOwnedAllowsWr()} but using subintervals
	 * for {@code a1, a2, a3}.
	 */
	@Test public void testRdOwnedAllowsWrWithSubintervals() {
		
		// |-a--|-a1-|--|-a2----|--|-a3-|-|
		//               |-a21-|
		//               |-a22-|

		final int a2children = 2;
		final List<Boolean> results = new ArrayList<Boolean>();
		
		Intervals.subinterval(new VoidSubinterval() { 
			@Override public void run(Interval a) {
				final DynamicGuard dg = new DynamicGuard();
				
				Intervals.subinterval(new VoidSubinterval() {					
					@Override public String toString() { return "a1"; }
					@Override public void run(Interval a1) {
						results.add(dg.isReadable());
						results.add(dg.isWritable());
						results.add(dg.isReadable());
					}
				});
				
				Intervals.subinterval(new VoidSubinterval() {					
					@Override public String toString() { return "a2"; }
					@Override public void run(Interval a2) {
						results.add(dg.isReadable());
						
						for(int i = 1; i <= a2children; i++) {
							final int num = i;
							new Interval(a2) {
								@Override public String toString() { return "a2"+num; }
								@Override protected void run() {
									results.add(dg.isReadable());
								}
							};
						}
					}
				});
				
				Intervals.subinterval(new VoidSubinterval() {					
					@Override public String toString() { return "a3"; }
					@Override public void run(Interval a3) {
						results.add(dg.isReadable());
						results.add(dg.isReadable());
						results.add(dg.isWritable());
					}					
				});
			}
		});
		
		assertEquals(3+1+a2children+3, results.size());
		for(int i = 0; i < results.size(); i++)
			assertEquals(true, results.get(i));
	}

	/**
	 * Similar to {@link #testRdOwnedAllowsWr()} but using subintervals
	 * only for {@code a2}. 
	 */
	@Test public void testRdOwnedAllowsWrWithA2Subinterval() {
		
		// |-a----|-a2----|-|
		//         |-a21-|
		//         |-a22-|

		final int a2children = 2;
		final List<Boolean> results = new ArrayList<Boolean>();
		
		Intervals.subinterval(new VoidSubinterval() { 
			@Override public void run(Interval a) {
				final DynamicGuard dg = new DynamicGuard();
				
				results.add(dg.isReadable());
				results.add(dg.isWritable());
				results.add(dg.isReadable());
				
				Intervals.subinterval(new VoidSubinterval() {					
					@Override public String toString() { return "a2"; }
					@Override public void run(Interval a2) {
						results.add(dg.isReadable());
						
						for(int i = 1; i <= a2children; i++) {
							final int num = i;
							new Interval(a2) {
								@Override public String toString() { return "a2"+num; }
								@Override protected void run() {
									results.add(dg.isReadable());
								}
							};
						}
					}
				});
				
				results.add(dg.isReadable());
				results.add(dg.isReadable());
				results.add(dg.isWritable());
			}
		});
		
		assertEquals(3+1+a2children+3, results.size());
		//for(int i = 0; i < results.size(); i++)
		//	assertEquals(true, results.get(i));
		
		int i = 0;
		assertEquals(true, results.get(i++));
		assertEquals(true, results.get(i++));
		assertEquals(true, results.get(i++));  // Should be in Wr Owned (a) state

		assertEquals(true, results.get(i++));  // Rd Shared (a2) state
		assertEquals(true, results.get(i++)); 
		assertEquals(true, results.get(i++)); 
		
		assertEquals(true, results.get(i++));  // Rd Shared (a2) becomes Rd Owned (a)
		assertEquals(true, results.get(i++));
		assertEquals(true, results.get(i++));
	}

	/**
	 * Similar to {@link #testRdOwnedAllowsWr()} except that
	 * we use a different bound on {@code a21} so that in 
	 * {@code a3} the guard is in the rd shared state.
	 */
	@Test public void testRdSharedDoesNotAllowWr() {
		
		// |-a-----------------------------|
		//   |-a1-| -> |-a2----| -> |-a3-|
		//              |-a21-| (bounded by a)

		final List<Boolean> results = new ArrayList<Boolean>();
		
		Intervals.subinterval(new VoidSubinterval() { 
			@Override public void run(final Interval a) {
				final DynamicGuard dg = new DynamicGuard();
				
				final Interval a1 = new Interval(a) {
					@Override public String toString() { return "a1"; }
					@Override protected void run() {
						results.add(dg.isReadable());
						results.add(dg.isWritable());
						results.add(dg.isReadable());
					}
				};
				
				final Interval a2 = new Interval(a) {					
					{ Intervals.addHb(a1.end, this.start); }
					@Override public String toString() { return "a2"; }
					@Override protected void run() {
						results.add(dg.isReadable());
						
						// Note: our BOUND is the a interval, even though we add
						// an HB that guarantees we finish by the end of a2:
						final Interval a2 = this;
						new Interval(a) {
							{ Intervals.addHb(end, a2.end); }
							@Override public String toString() { return "a21"; }
							@Override protected void run() {
								results.add(dg.isReadable());
							}
						};
					}
				};
				
				new Interval(a) {										
					{ Intervals.addHb(a2.end, this.start); }
					@Override public String toString() { return "a3"; }					
					@Override protected void run() {
						results.add(dg.isReadable());
						results.add(dg.isReadable());
						results.add(dg.isWritable());
					}					
				};
			}
		});
		
		assertEquals(3+2+3, results.size());
		
		int i = 0;
		assertEquals(true, results.get(i++));
		assertEquals(true, results.get(i++));
		assertEquals(true, results.get(i++));  // Should be in Wr Owned (a1) state

		assertEquals(true, results.get(i++));  // Now Rd Owned (a2)
		assertEquals(true, results.get(i++));  // Now Rd Shared (a)

		assertEquals(true, results.get(i++));  // Now Rd Shared (a)
		assertEquals(true, results.get(i++));  // Now Rd Shared (a)
		assertEquals(false, results.get(i++)); // Cannot convert to Wr Owned state now!
	}
	
	/**
	 * Test that if the current Rd Owner has terminated,
	 * then next reader also becomes Rd Owner
	 */
	@Test public void testRdOwnedToRdOwned() {
		
		// |-a--------------------------|
		//   |-a1-| -> |-a2-| -> |-a3-|

		final List<Boolean> results = new ArrayList<Boolean>();
		
		Intervals.subinterval(new VoidSubinterval() { 
			@Override public void run(final Interval a) {
				final DynamicGuard dg = new DynamicGuard();
				
				final Interval a1 = new Interval(a) {
					@Override public String toString() { return "a1"; }
					@Override protected void run() {
						results.add(dg.isWritable());
					}
				};
				
				final Interval a2 = new Interval(a) {					
					{ Intervals.addHb(a1.end, this.start); }
					@Override public String toString() { return "a2"; }
					@Override protected void run() {
						results.add(dg.isReadable());
					}
				};
				
				new Interval(a) {										
					{ Intervals.addHb(a2.end, this.start); }
					@Override public String toString() { return "a3"; }					
					@Override protected void run() {
						results.add(dg.isReadable());
						results.add(dg.isWritable());
					}					
				};
			}
		});
		
		assertEquals(1+1+2, results.size());
		
		int i = 0;
		assertEquals(true, results.get(i++));  // Wr Owned (a1)
		
		assertEquals(true, results.get(i++));  // Rd Owned (a2)
		
		assertEquals(true, results.get(i++));  // Rd Owned (a3)
		assertEquals(true, results.get(i++));  // Wr Owned (a3)
	}

}
