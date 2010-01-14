package ch.ethz.intervals;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CountDownLatch;

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
			return new DgInterval(dep, name, flags, null, null);
		}
		
		public Interval create(Dependency dep, String name, int flags, CountDownLatch await, CountDownLatch signal) {
			return new DgInterval(dep, name, flags, await, signal);
		}
		
		class DgInterval extends Interval {
			
			final String name;
			final int flags;
			final CountDownLatch await;
			final CountDownLatch signal;

			public DgInterval(
					Dependency dep, 
					String name, 
					int flags,
					CountDownLatch await,
					CountDownLatch signal) 
			{
				super(dep, name);
				this.name = name;
				this.flags = flags;
				this.await = await;
				this.signal = signal;
				
				if((flags & FLAG_LCK) != 0)
					Intervals.addExclusiveLock(this, dg);
			}

			@Override
			protected void run() {
				// Sometimes we want to force an ordering.  This can be hard
				// to do, so we cheat:
				if(await != null)
					try {
						await.await();
					} catch (InterruptedException e) {}
				
				if((flags & FLAG_RD1) != 0) 
					results.put(name + ".rd1", dg.isReadable());
				if((flags & FLAG_WR1) != 0) 
					results.put(name + ".wr1", dg.isWritable());
				if((flags & FLAG_RD2) != 0) 
					results.put(name + ".rd2", dg.isReadable());
				if((flags & FLAG_WR2) != 0) 
					results.put(name + ".wr2", dg.isWritable());

				if(signal != null)
					await.countDown();
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
				
				final Interval a1 = new Interval(a, "a1") {
					@Override protected void run() {
						results.add(dg.isReadable());
						results.add(dg.isWritable());
						results.add(dg.isReadable());
					}
				};
				
				final Interval a2 = new Interval(a, "a2") {					
					{ Intervals.addHb(a1.end, this.start); }
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
				
				new Interval(a, "a3") {										
					{ Intervals.addHb(a2.end, this.start); }
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
							new Interval(a2, "a2"+i) {
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
							new Interval(a2, "a2"+i) {
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

		assertEquals(true, results.get(i++));  // Rd Owned (a2) state is pushed on
		assertEquals(true, results.get(i++));  // Rd Owned (a21) 
		assertEquals(true, results.get(i++));  // Rd Shared (a2.end)
		
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
				
				final Interval a1 = new Interval(a, "a1") {
					@Override protected void run() {
						results.add(dg.isReadable());
						results.add(dg.isWritable());
						results.add(dg.isReadable());
					}
				};
				
				final Interval a2 = new Interval(a, "a2") {					
					{ Intervals.addHb(a1.end, this.start); }
					@Override protected void run() {
						results.add(dg.isReadable());
					}
				};

				// a3 runs in parallel with a2 (in theory) but
				// we use a TESTS edge to force it to run afterwards:
				final Interval a3 = new Interval(a, "a3") {
					{ 
						Intervals.addHb(a1.end, this.start);
						a2.end.addEdgeAndAdjust(start, ChunkList.TESTS);
					}
					@Override protected void run() {
						results.add(dg.isReadable());
					}
				};
				
				new Interval(a, "a4") {										
					{ Intervals.addHb(a3.end, this.start); }
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
	
	@Test public void testLockingIntervalDoesNotContendWithChildren() {
		final DgIntervalFactory f = new DgIntervalFactory();
		
		Intervals.subinterval(new VoidSubinterval() { 
			@Override public void run(final Interval a) {				
				Interval withLock1 = f.create(a, "withLock1", FLAG_LCK|FLAG_RD1);
				f.create(withLock1, "noLock", FLAG_WR1);
			}
		});		
		
		assertEquals(2, f.results.size());
		
		Assert.assertTrue(
				f.result("withLock1.rd1") &&
				f.result("noLock.wr1"));
	}
	
	@Test public void testReadsWithoutWrites() {
		final DgIntervalFactory f = new DgIntervalFactory();
		
		Intervals.subinterval(new VoidSubinterval() { 
			@Override public void run(final Interval a) {				
				f.create(a, "reader0", FLAG_RD1);
				f.create(a, "reader1", FLAG_RD1);
				f.create(a, "reader2", FLAG_RD1);
			}
		});		
		
		assertEquals(3, f.results.size());
		
		Assert.assertTrue(
				f.result("reader0.rd1") && 
				f.result("reader1.rd1") && 
				f.result("reader2.rd1"));		
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
	
	@Test public void testWriteHandoff() {
		final DgIntervalFactory f = new DgIntervalFactory();
		
		Intervals.subinterval(new VoidSubinterval() { 
			@Override public void run(final Interval a) {				
				Interval wrParent = f.create(a, "wrParent", FLAG_WR1);				
				Interval wrChild1 = f.create(wrParent, "wrChild1", FLAG_WR1);
				Interval wrChild2 = f.create(wrParent, "wrChild2", FLAG_WR1);
				Intervals.addHb(wrChild1, wrChild2);
			}
		});		
		
		assertEquals(3, f.results.size());
		
		Assert.assertTrue(
				f.result("wrParent.wr1") &&
				f.result("wrChild1.wr1") && 
				f.result("wrChild2.wr1"));
	}
	
	@Test public void testWriteHandoffWithStrayReader() {
		final DgIntervalFactory f = new DgIntervalFactory();
		
		Intervals.subinterval(new VoidSubinterval() { 
			@Override public void run(final Interval a) {				
				Interval wrParent = f.create(a, "wrParent", FLAG_WR1);				
				Interval wrChild1 = f.create(wrParent, "wrChild1", FLAG_WR1);
				Interval wrChild2 = f.create(wrParent, "wrChild2", FLAG_WR1);
				f.create(wrParent, "rdChild", FLAG_RD1);
				Intervals.addHb(wrChild1, wrChild2);
			}
		});		
		
		assertEquals(4, f.results.size());
		
		Assert.assertTrue(
				f.result("wrParent.wr1") &&
				(
						(f.result("wrChild1.wr1") && f.result("wrChild2.wr1") && !f.result("rdChild.rd1")) ||
						(f.result("wrChild1.wr1") && !f.result("wrChild2.wr1") && f.result("rdChild.rd1")) || 
						(!f.result("wrChild1.wr1") && !f.result("wrChild2.wr1") && f.result("rdChild.rd1"))
				));
	}
	
	boolean testUnlockLockRaceHelper(final boolean lockFirst, final int unlockFlag, String accName) {
		final DgIntervalFactory f = new DgIntervalFactory();
		
		try {
			Intervals.subinterval(new VoidSubinterval() { 
				@Override public String toString() { return "parent"; }
				@Override public void run(final Interval parent) {
					Interval lockChild = f.create(parent, "lockChild", FLAG_LCK); 
					Interval unlockChild = f.create(parent, "unlockChild", unlockFlag); 
					
					// Use speculative flag to enforce an ordering without actually creating HB relations:
					if(lockFirst) 
						lockChild.end.addEdgeAndAdjust(unlockChild.start, ChunkList.TESTS);
					else
						unlockChild.end.addEdgeAndAdjust(lockChild.start, ChunkList.TESTS);
				}
			});

			// If lock fails, an exception is thrown.  Since no exception, write
			// should have failed:
			Assert.assertTrue(!f.result("unlockChild."+accName));	
			return true; // unlock failed
		} catch (RethrownException e) {
			Assert.assertTrue(f.result("unlockChild."+accName));			
			
			Assert.assertTrue(
					"Not a DataRaceException:"+e.getCause(), 
					e.getCause() instanceof DataRaceException);
			
			DataRaceException dre = (DataRaceException)e.getCause();
			Assert.assertEquals(f.dg, dre.dg);
			Assert.assertEquals(DataRaceException.Role.LOCK, dre.acc);
			Assert.assertEquals("lockChild", dre.interloper.toString());
			//FIX Assert.assertEquals("unlockChild", dre.owner);
			return false; // lock failed
		}
	}

	
	@Test public void testLockWriteRace() {
		boolean ufRd = testUnlockLockRaceHelper(true, FLAG_RD1, "rd1");
		boolean ufWr = testUnlockLockRaceHelper(true, FLAG_WR1, "wr1");
		boolean lfRd = testUnlockLockRaceHelper(false, FLAG_RD1, "rd1");
		boolean lfWr = testUnlockLockRaceHelper(false, FLAG_WR1, "wr1");
		
		// Right now, it's deterministic which will run first, so assert
		// that we've tried out both code paths:
		Assert.assertEquals(ufRd, ufWr);
		Assert.assertEquals(lfRd, lfWr);
		Assert.assertEquals(lfRd, !ufRd);
	}

	@Test public void testMustHappenAfterPrevWr() {
		final DgIntervalFactory f = new DgIntervalFactory();
		
		Intervals.subinterval(new VoidSubinterval() {
			@Override public String toString() { return "outer"; }
			@Override public void run(Interval subinterval) {
				Interval wr = f.create(subinterval, "wr", FLAG_WR1, null, null);
				Interval ro = f.create(subinterval, "rdOwner", FLAG_RD1, null, null);
				Intervals.addHb(wr, ro);

				// Note: rp is parallel to wr and ro, but "happens" to come after ro:
				Interval rp = f.create(subinterval, "rdPar", FLAG_RD1, null, null);
				ro.end.addEdgeAndAdjust(rp.start, ChunkList.TESTS);
			}
		});
		
		assertEquals(3, f.results.size());
		
		Assert.assertTrue(
				f.results.toString(),
				f.result("wr.wr1") &&
				f.result("rdOwner.rd1") &&
				!f.result("rdPar.rd1"));
	}

}
