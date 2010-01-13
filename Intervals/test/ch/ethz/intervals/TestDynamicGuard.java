package ch.ethz.intervals;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Assert;
import org.junit.Test;

public class TestDynamicGuard {
	
	public static final int FLAG_LCK = 1;
	public static final int FLAG_RD1 = 2;
	public static final int FLAG_WR1 = 4;
	public static final int FLAG_RD2 = 8;
	public static final int FLAG_WR2 = 16;
	
	class DgIntervalFactory {
		public final DynamicGuard dg = new DynamicGuard();
		public final Map<String, Boolean> results = 
			Collections.synchronizedMap(new HashMap<String, Boolean>());
		
		public boolean result(String name) {
			return results.get(name).booleanValue();
		}
		
		public Interval create(Dependency dep, String name, int flags) {
			return new DgInterval(dep, name, flags);
		}
		
		class DgInterval extends Interval {
			
			final String name;
			final int flags;

			public DgInterval(
					Dependency dep, 
					String name, 
					int flags) 
			{
				super(dep, name);
				this.name = name;
				this.flags = flags;
				
				if((flags & FLAG_LCK) != 0)
					Intervals.addExclusiveLock(this, dg);
			}

			@Override
			protected void run() {
				if((flags & FLAG_RD1) != 0) 
					results.put(name + ".rd1", dg.isReadable());
				if((flags & FLAG_WR1) != 0) 
					results.put(name + ".wr1", dg.isWritable());
				if((flags & FLAG_RD2) != 0) 
					results.put(name + ".rd2", dg.isReadable());
				if((flags & FLAG_WR2) != 0) 
					results.put(name + ".wr2", dg.isWritable());
			}
			
		}		
	}
	

	
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

		assertEquals(true, results.get(i++));  // State machine permits 
		assertEquals(false, results.get(i++)); // subintervals on the same line,
		assertEquals(false, results.get(i++)); // but not asynchronous ones.    
		
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
	
	@Test public void testTwoWritersObtainingLocks() {
		final List<Boolean> results = Collections.synchronizedList(new ArrayList<Boolean>());
		
		Intervals.subinterval(new VoidSubinterval() { 
			@Override public void run(final Interval a) {
				final DynamicGuard dg = new DynamicGuard();
				
				new Interval(a, "withLock1") {
					{ Intervals.addExclusiveLock(this, dg); }
					@Override protected void run() {
						results.add(dg.isWritable());
					}
				};
				
				new Interval(a, "withLock2") {					
					{ Intervals.addExclusiveLock(this, dg); }
					@Override protected void run() {
						results.add(dg.isWritable());
					}
				};
			}
		});		
		
		assertEquals(2, results.size());
		
		int i = 0;
		assertEquals(true, results.get(i++));
		assertEquals(true, results.get(i++));
	}
	
	@Test public void testLockingIntervalContendsWithChildren() {
		final DgIntervalFactory f = new DgIntervalFactory();
		
		Intervals.subinterval(new VoidSubinterval() { 
			@Override public void run(final Interval a) {				
				Interval withLock1 = f.create(a, "withLock1", FLAG_LCK|FLAG_RD1);
				Interval noLock = f.create(withLock1, "noLock", FLAG_WR1);
			}
		});		
		
		assertEquals(2, f.results.size());
		
		Assert.assertTrue(
				f.result("withLock1.rd1") == !f.result("noLock.wr1"));
	}
	
	@Test public void testCooperatingChildren() {
		final DgIntervalFactory f = new DgIntervalFactory();
		
		Intervals.subinterval(new VoidSubinterval() { 
			@Override public void run(final Interval a) {				
				Interval withLock1 = f.create(a, "withLock1", FLAG_LCK);
				
				Interval readers = f.create(withLock1, "readers", 0);
				Interval reader0 = f.create(readers, "reader0", FLAG_RD1);
				Interval reader1 = f.create(readers, "reader1", FLAG_RD1);
				Interval reader2 = f.create(readers, "reader2", FLAG_RD1);
				
				Interval writers = f.create(withLock1, "writers", 0);
				Interval writer0 = f.create(writers, "writer0", FLAG_RD1|FLAG_WR2);
				
				Intervals.addHb(readers.end, writers.start);
			}
		});		
		
		assertEquals(5, f.results.size());
		
		Assert.assertTrue(
				f.result("reader0.rd1") && 
				f.result("reader1.rd1") && 
				f.result("reader2.rd1") && 
				f.result("writer0.rd1") &&
				f.result("writer0.wr2"));
	}
	
	@Test public void testGoToLock() {
		final DgIntervalFactory f = new DgIntervalFactory();
		
		Intervals.subinterval(new VoidSubinterval() { 
			@Override public void run(final Interval a) {				
				Interval withLock1 = f.create(a, "withLock1", FLAG_LCK);
				
				Interval readers = f.create(withLock1, "readers", 0);
				Interval reader0 = f.create(readers, "reader0", FLAG_RD1);
				Interval reader1 = f.create(readers, "reader1", FLAG_RD1);
				Interval reader2 = f.create(readers, "reader2", FLAG_RD1);
				
				Interval writers = f.create(withLock1, "writers", 0);
				Interval writer0 = f.create(writers, "writer0", FLAG_RD1|FLAG_WR2);
				
				Intervals.addHb(readers.end, writers.start);
			}
		});		
		
		assertEquals(5, f.results.size());
		
		Assert.assertTrue(
				f.result("reader0.rd1") && 
				f.result("reader1.rd1") && 
				f.result("reader2.rd1") && 
				f.result("writer0.rd1") &&
				f.result("writer0.wr2"));
	}
	
}
