package ch.ethz.intervals;

import static ch.ethz.intervals.util.ChunkList.TEST_EDGE;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Assert;
import org.junit.Test;

import ch.ethz.intervals.IntervalException.DataRace;
import ch.ethz.intervals.guard.ReadSummarizingDynamicGuard;
import ch.ethz.intervals.guard.ReadTrackingDynamicGuard;
import ch.ethz.intervals.guard.Guard;
import ch.ethz.intervals.util.ChunkList;

public class TestDynamicGuard {
	
	public static final int FLAG_LCK1FOR1 = 1 << 0;
	public static final int FLAG_LCK1FOR2 = 1 << 1;
	public static final int FLAG_RD1 = 1 << 2;
	public static final int FLAG_WR1 = 1 << 3;
	
	public static final int FLAG_LCK2FOR1 = 1 << 10;
	public static final int FLAG_LCK2FOR2 = 1 << 11;
	public static final int FLAG_RD2 = 1 << 12;
	public static final int FLAG_WR2 = 1 << 13;	
	
	boolean isWritable(Guard g) {
		try {			
			boolean res = Intervals.checkWritable(g);
			assert res;
			return true;
		} catch (IntervalException exc) {
			return false;
		}
	}

	boolean isReadable(Guard g) {
		try {
			boolean res = Intervals.checkReadable(g);
			assert res;
			return true;
		} catch (IntervalException exc) {
			return false;
		}
	}

	class DgIntervalFactory {
		public final Lock l1 = new Lock("l1");
		public final Lock l2 = new Lock("l2");
		
		public final ReadTrackingDynamicGuard dg1 = new ReadTrackingDynamicGuard("dg1");
		
		public final ReadSummarizingDynamicGuard dg2 = new ReadSummarizingDynamicGuard("dg2");
		
		public final Map<String, Boolean> results = 
			Collections.synchronizedMap(new HashMap<String, Boolean>());
		int resultCheckedCounter;
		
		public boolean result(String name) {
			resultCheckedCounter++;
			return results.get(name).booleanValue();
		}
		
		public boolean allResultsChecked() {
			return resultCheckedCounter == results.size();
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
				
				if((flags & FLAG_LCK1FOR1) != 0)
					Intervals.addExclusiveLock(this, l1, dg1);
				if((flags & FLAG_LCK1FOR2) != 0)
					Intervals.addExclusiveLock(this, l1, dg2);
				
				if((flags & FLAG_LCK2FOR1) != 0)
					Intervals.addExclusiveLock(this, l2, dg1);
				if((flags & FLAG_LCK2FOR2) != 0)
					Intervals.addExclusiveLock(this, l2, dg2);
			}

			@Override
			protected void run() {
				if((flags & FLAG_RD1) != 0) 
					results.put(name + ".rd1", isReadable(dg1));
				if((flags & FLAG_WR1) != 0) 
					results.put(name + ".wr1", isWritable(dg1));
				
				if((flags & FLAG_RD2) != 0) 
					results.put(name + ".rd2", isReadable(dg2));
				if((flags & FLAG_WR2) != 0) 
					results.put(name + ".wr2", isWritable(dg2));
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
				final ReadTrackingDynamicGuard dg = new ReadTrackingDynamicGuard();
				
				final Interval a1 = new Interval(a, "a1") {
					@Override protected void run() {
						results.add(isReadable(dg));
						results.add(isWritable(dg));
						results.add(isReadable(dg));
					}
				};
				
				final Interval a2 = new Interval(a, "a2") {					
					{ Intervals.addHb(a1.end, this.start); }
					@Override protected void run() {
						results.add(isReadable(dg));
						
						for(int i = 1; i <= a2children; i++) {
							final int num = i;
							new Interval(this) {
								@Override public String toString() { return "a2"+num; }
								@Override protected void run() {
									results.add(isReadable(dg));
								}
							};
						}
					}
				};
				
				new Interval(a, "a3") {										
					{ Intervals.addHb(a2.end, this.start); }
					@Override protected void run() {
						results.add(isReadable(dg));
						results.add(isReadable(dg));
						results.add(isWritable(dg));
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
				final ReadTrackingDynamicGuard dg = new ReadTrackingDynamicGuard();
				
				Intervals.subinterval(new VoidSubinterval() {
					@Override public String toString() { return "a1"; }
					@Override public void run(Interval a1) {
						results.add(isReadable(dg));
						results.add(isWritable(dg));
						results.add(isReadable(dg));
					}
				});
				
				Intervals.subinterval(new VoidSubinterval() {					
					@Override public String toString() { return "a2"; }
					@Override public void run(Interval a2) {
						results.add(isReadable(dg));
						
						for(int i = 1; i <= a2children; i++) {
							new Interval(a2, "a2"+i) {
								@Override protected void run() {
									results.add(isReadable(dg));
								}
							};
						}
					}
				});
				
				Intervals.subinterval(new VoidSubinterval() {					
					@Override public String toString() { return "a3"; }
					@Override public void run(Interval a3) {
						results.add(isReadable(dg));
						results.add(isReadable(dg));
						results.add(isWritable(dg));
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
				final ReadTrackingDynamicGuard dg = new ReadTrackingDynamicGuard();
				
				results.add(isReadable(dg));
				results.add(isWritable(dg));
				results.add(isReadable(dg));
				
				Intervals.subinterval(new VoidSubinterval() {					
					@Override public String toString() { return "a2"; }
					@Override public void run(Interval a2) {
						results.add(isReadable(dg));
						
						for(int i = 1; i <= a2children; i++) {
							new Interval(a2, "a2"+i) {
								@Override protected void run() {
									results.add(isReadable(dg));
								}
							};
						}
					}
				});
				
				results.add(isReadable(dg));
				results.add(isReadable(dg));
				results.add(isWritable(dg));
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
				final ReadTrackingDynamicGuard dg = new ReadTrackingDynamicGuard();
				
				final Interval a1 = new Interval(a, "a1") {
					@Override protected void run() {
						results.add(isReadable(dg));
						results.add(isWritable(dg));
						results.add(isReadable(dg));
					}
				};
				
				final Interval a2 = new Interval(a, "a2") {					
					{ Intervals.addHb(a1.end, this.start); }
					@Override protected void run() {
						results.add(isReadable(dg));
					}
				};

				// a3 runs in parallel with a2 (in theory) but
				// we use a TESTS edge to force it to run afterwards:
				final Interval a3 = new Interval(a, "a3") {
					{ 
						Intervals.addHb(a1.end, this.start);
						a2.end.addEdgeAndAdjust(start, ChunkList.TEST_EDGE);
					}
					@Override protected void run() {
						results.add(isReadable(dg));
					}
				};
				
				new Interval(a, "a4") {										
					{ Intervals.addHb(a3.end, this.start); }
					@Override protected void run() {
						results.add(isReadable(dg));
						results.add(isReadable(dg));
						results.add(isWritable(dg));
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
	 * Similar to {@link #testRdOwnedAllowsWr()} except that
	 * we use a different bound on {@code a21} so that in 
	 * {@code a3} the guard is in the rd shared state.
	 */
	@Test public void testRdSharedBoundedByWr() {
		
		// |-a-----------------------------|
		//          |-a1-| 
		//          |-a2-|   |-a4-|
		//          |-a3-|
		//                 |-b-----------------------------|

		final DgIntervalFactory f = new DgIntervalFactory();
		
		Intervals.subinterval(new VoidSubinterval() { 
			@Override public void run(final Interval outer) {				
				Interval a = f.create(outer, "a", FLAG_WR1);
				Interval a1 = f.create(a, "a1", FLAG_RD1);
				Interval a2 = f.create(a, "a2", FLAG_RD1);
				Interval a3 = f.create(a, "a3", FLAG_RD1);
				Interval a4 = f.create(a, "a4", FLAG_WR1);
				Intervals.addHb(a1, a4);
				Intervals.addHb(a2, a4);
				Intervals.addHb(a3, a4);
				
				Interval b = f.create(outer, "b", FLAG_RD1);
				
				a1.end.addEdgeAndAdjust(b.start, TEST_EDGE);
				b.start.addEdgeAndAdjust(a4.start, TEST_EDGE);
			}
		});
		
		Assert.assertTrue(f.result("a.wr1"));
		Assert.assertTrue(f.result("a1.rd1"));
		Assert.assertTrue(f.result("a2.rd1"));
		Assert.assertTrue(f.result("a3.rd1"));
		Assert.assertTrue(f.result("a4.wr1"));
		Assert.assertFalse(f.result("b.rd1"));
		
		Assert.assertTrue(f.allResultsChecked());
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
				final ReadTrackingDynamicGuard dg = new ReadTrackingDynamicGuard();
				
				final Interval a1 = new Interval(a) {
					@Override public String toString() { return "a1"; }
					@Override protected void run() {
						results.add(isWritable(dg));
					}
				};
				
				final Interval a2 = new Interval(a) {					
					{ Intervals.addHb(a1.end, this.start); }
					@Override public String toString() { return "a2"; }
					@Override protected void run() {
						results.add(isReadable(dg));
					}
				};
				
				new Interval(a) {										
					{ Intervals.addHb(a2.end, this.start); }
					@Override public String toString() { return "a3"; }					
					@Override protected void run() {
						results.add(isReadable(dg));
						results.add(isWritable(dg));
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
		final DgIntervalFactory f = new DgIntervalFactory();
		
		Intervals.subinterval(new VoidSubinterval() { 
			@Override public void run(final Interval outer) {
				f.create(outer, "a", FLAG_LCK1FOR1|FLAG_WR1);
				f.create(outer, "b", FLAG_LCK1FOR1|FLAG_WR1);
			}
		});		
		
		Assert.assertTrue(f.result("a.wr1"));		
		Assert.assertTrue(f.result("b.wr1"));		
		Assert.assertTrue(f.allResultsChecked());
	}
	
	@Test public void testLockingIntervalDoesNotContendWithChildren() {
		final DgIntervalFactory f = new DgIntervalFactory();
		
		Intervals.subinterval(new VoidSubinterval() { 
			@Override public void run(final Interval a) {				
				Interval withLock1 = f.create(a, "withLock1", FLAG_LCK1FOR1|FLAG_RD1);
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
				Interval withLock1 = f.create(a, "withLock1", FLAG_LCK1FOR1);
				
				Interval readers = f.create(withLock1, "readers", 0);
				Interval reader0 = f.create(readers, "reader0", FLAG_RD1);
				Interval reader1 = f.create(readers, "reader1", FLAG_RD1);
				Interval reader2 = f.create(readers, "reader2", FLAG_RD1);
				
				Interval writers = f.create(withLock1, "writers", 0);
				Interval writer0 = f.create(writers, "writer0", FLAG_RD1|FLAG_WR1);
				
				Intervals.addHb(readers.end, writers.start);
			}
		});		
		
		assertEquals(5, f.results.size());
		
		Assert.assertTrue(
				f.result("reader0.rd1") && 
				f.result("reader1.rd1") && 
				f.result("reader2.rd1") && 
				f.result("writer0.rd1") &&
				f.result("writer0.wr1"));
	}
	
	@Test public void testGoToLock() {
		final DgIntervalFactory f = new DgIntervalFactory();
		
		Intervals.subinterval(new VoidSubinterval() { 
			@Override public void run(final Interval a) {				
				Interval withLock1 = f.create(a, "withLock1", FLAG_LCK1FOR1);
				
				Interval readers = f.create(withLock1, "readers", 0);
				Interval reader0 = f.create(readers, "reader0", FLAG_RD1);
				Interval reader1 = f.create(readers, "reader1", FLAG_RD1);
				Interval reader2 = f.create(readers, "reader2", FLAG_RD1);
				
				Interval writers = f.create(withLock1, "writers", 0);
				Interval writer0 = f.create(writers, "writer0", FLAG_RD1|FLAG_WR1);
				
				Intervals.addHb(readers.end, writers.start);
			}
		});		
		
		assertEquals(5, f.results.size());
		
		Assert.assertTrue(
				f.result("reader0.rd1") && 
				f.result("reader1.rd1") && 
				f.result("reader2.rd1") && 
				f.result("writer0.rd1") &&
				f.result("writer0.wr1"));
	}	
	
	@Test public void testTrackingAndSummarizingManyReaders() {
		final DgIntervalFactory f = new DgIntervalFactory();
		
		Intervals.subinterval(new VoidSubinterval() { 
			@Override public void run(final Interval a) {		
				Interval b1 = f.create(a, "b1", 0);
				
				Interval wr1 = f.create(b1, "wr1", FLAG_WR1|FLAG_WR2);
				
				Interval reader0 = f.create(b1, "reader0", FLAG_RD1|FLAG_RD2);
				Interval reader1 = f.create(b1, "reader1", FLAG_RD1|FLAG_RD2);
				Intervals.addHb(wr1, reader0);
				Intervals.addHb(wr1, reader1);
				
				Interval wr2 = f.create(b1, "wr2", FLAG_WR1|FLAG_WR2);
				Intervals.addHb(reader0, wr2);
				Intervals.addHb(reader1, wr2);	
				
				Interval b2 = f.create(a, "b2", FLAG_WR1|FLAG_WR2);
				Intervals.addHb(b1, b2);
			}
		});		
		
		Assert.assertTrue(
				
				// The tracking version succeeds:
				f.result("wr1.wr1") &&
				f.result("reader0.rd1") &&
				f.result("reader1.rd1") &&
				f.result("wr2.wr1") &&
				f.result("b2.wr1") &&
				
				// The summarizing version fails to
				// allow wr2, because the mutual
				// bound of reader0 and reader1 is b1.end,
				// but it does allow b2:
				f.result("wr1.wr2") &&
				f.result("reader0.rd2") &&
				f.result("reader1.rd2") &&
				!f.result("wr2.wr2") &&
				f.result("b2.wr2") &&
				
				f.allResultsChecked());
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
					Interval lockChild = f.create(parent, "lockChild", FLAG_LCK1FOR1); 
					Interval unlockChild = f.create(parent, "unlockChild", unlockFlag); 
					
					if(lockFirst) 
						lockChild.end.addEdgeAndAdjust(unlockChild.start, ChunkList.TEST_EDGE);
					else
						unlockChild.end.addEdgeAndAdjust(lockChild.start, ChunkList.TEST_EDGE);
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
					e.getCause() instanceof IntervalException.DataRace);
			
			IntervalException.DataRace dre = (IntervalException.DataRace)e.getCause();
			Assert.assertEquals(f.dg1, dre.dg);
			Assert.assertEquals("LOCK(l1)", dre.interloperRole.toString());
			Assert.assertEquals("lockChild", dre.interloper.toString());
			Assert.assertEquals("unlockChild.end", dre.ownerBound.toString());
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
				Interval wr = f.create(subinterval, "wr", FLAG_WR1);
				Interval ro = f.create(subinterval, "rdOwner", FLAG_RD1);
				Intervals.addHb(wr, ro);

				// Note: rp is parallel to wr and ro, but "happens" to come after ro:
				Interval rp = f.create(subinterval, "rdPar", FLAG_RD1);
				ro.end.addEdgeAndAdjust(rp.start, ChunkList.TEST_EDGE);
			}
		});
		
		assertEquals(3, f.results.size());
		
		Assert.assertTrue(
				f.results.toString(),
				f.result("wr.wr1") &&
				f.result("rdOwner.rd1") &&
				!f.result("rdPar.rd1"));
	}

	@Test public void testEmbedUnembedSuccessfully() {
		final DgIntervalFactory f = new DgIntervalFactory();
		
		// |-wr1a-| -> |-l1a-| 
		//                      |-l1b-| -> |-wr1b-|
		//
		// Allowed because wr1a and wr1b are ordered through l2.
		
		Intervals.subinterval(new VoidSubinterval() {
			@Override public String toString() { return "outer"; }
			@Override public void run(Interval subinterval) {
				Interval wr1a = f.create(subinterval, "wr1a", FLAG_WR1);
				Interval l2a = f.create(subinterval, "l1a", FLAG_LCK1FOR1);
				Intervals.addHb(wr1a, l2a);

				Interval l2b = f.create(subinterval, "l1b", FLAG_LCK1FOR1);
				Interval wr1b = f.create(subinterval, "wr1b", FLAG_WR1);
				Intervals.addHb(l2b, wr1b);

				l2a.end.addEdgeAndAdjust(l2b.start, TEST_EDGE);
			}
		});
		
		Assert.assertTrue(
				f.results.toString(),
				f.result("wr1a.wr1") &&
				f.result("wr1b.wr1") &&
				f.allResultsChecked());
	}

	@Test public void testEmbedUnembedExtraLock() {
		final DgIntervalFactory f = new DgIntervalFactory();
		
		//
		// |-wr1a-| -> |-l2a-| 
		//                      |-l2b-| -> |-wr1b-|
		//                                     |-l3b-|
		//
		// Illegal because l3b and wr1b conflict.
		
		try {
			Intervals.subinterval(new VoidSubinterval() {
				@Override public String toString() { return "outer"; }
				@Override public void run(Interval subinterval) {
					Interval wr1a = f.create(subinterval, "wr1a", FLAG_WR1);
					Interval l2a = f.create(subinterval, "l2a", FLAG_LCK1FOR1);
					Intervals.addHb(wr1a, l2a);
	
					Interval l2b = f.create(subinterval, "l2b", FLAG_LCK1FOR1);
					Interval wr1b = f.create(subinterval, "wr1b", FLAG_WR1);
					Intervals.addHb(l2b, wr1b);
	
					Interval l2c = f.create(subinterval, "l2c", FLAG_WR1|FLAG_LCK1FOR1);
					
					l2a.end.addEdgeAndAdjust(l2b.start, TEST_EDGE);
					wr1b.end.addEdgeAndAdjust(l2c.start, TEST_EDGE);
				}
			});
			Assert.fail("l2c did not fail");
		} catch (RethrownException err) {
			IntervalException.DataRace dr = (DataRace) err.getCause();
			Assert.assertEquals("dg1", dr.dg.toString());
			Assert.assertEquals("LOCK(l1)", dr.interloperRole.toString()); 
			Assert.assertEquals("l2c", dr.interloper.toString()); 
			Assert.assertEquals(DataRace.WRITE, dr.ownerRole); 
			Assert.assertEquals("wr1b.end", dr.ownerBound.toString()); 
		}
		
		Assert.assertTrue(
				f.results.toString(),
				f.result("wr1a.wr1") &&
				f.result("wr1b.wr1") &&
				f.allResultsChecked());
	}
	
	@Test public void testEmbedUnembedExtraLockWithoutLockableBy() {
		final DgIntervalFactory f = new DgIntervalFactory();
		
		//
		// |-wr1a-| -> |-l2a-| 
		//                      |-l2b-| -> |-wr1b-|
		//                                     |-l2c-|
		//
		// In contrast to testEmbedUnembedExtraLock, 
		// Legal because l2c acquires the same lock but for
		// a different guard.
		
		Intervals.subinterval(new VoidSubinterval() {
			@Override public String toString() { return "outer"; }
			@Override public void run(Interval subinterval) {
				Interval wr1a = f.create(subinterval, "wr1a", FLAG_WR1);
				Interval l2a = f.create(subinterval, "l2a", FLAG_LCK1FOR1);
				Intervals.addHb(wr1a, l2a);

				Interval l2b = f.create(subinterval, "l2b", FLAG_LCK1FOR1);
				Interval wr1b = f.create(subinterval, "wr1b", FLAG_WR1);
				Intervals.addHb(l2b, wr1b);

				Interval l2c = f.create(subinterval, "l2c", FLAG_WR2|FLAG_LCK1FOR2);
				
				l2a.end.addEdgeAndAdjust(l2b.start, TEST_EDGE);
				wr1b.end.addEdgeAndAdjust(l2c.start, TEST_EDGE);
			}
		});
		
		Assert.assertTrue(
				f.results.toString(),
				f.result("wr1a.wr1") &&
				f.result("wr1b.wr1") &&
				f.result("l2c.wr2") &&
				f.allResultsChecked());
	}

	@Test public void testUnorderedWriteAfterUnembed() {
		final DgIntervalFactory f = new DgIntervalFactory();
		
		Intervals.subinterval(new VoidSubinterval() {
			@Override public String toString() { return "outer"; }
			@Override public void run(Interval subinterval) {
				Interval wr1a = f.create(subinterval, "wr1a", FLAG_WR1);
				Interval em = f.create(subinterval, "em", FLAG_LCK1FOR1|FLAG_WR1);
				Intervals.addHb(wr1a, em);

				Interval unem = f.create(subinterval, "unem", FLAG_LCK1FOR1|FLAG_WR1);
				em.end.addEdgeAndAdjust(unem.start, TEST_EDGE);
				
				// Note: this write is not ordered with respect to the unembed.
				Interval wr1b = f.create(subinterval, "wr1b", FLAG_WR1);
				unem.end.addEdgeAndAdjust(wr1b.start, TEST_EDGE);
			}
		});
		
		Assert.assertTrue(
				f.results.toString(),
				f.result("wr1a.wr1") &&
				f.result("em.wr1") &&
				f.result("unem.wr1") &&
				!f.result("wr1b.wr1") &&
				f.allResultsChecked());
	}
}
