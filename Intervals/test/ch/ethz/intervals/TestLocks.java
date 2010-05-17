package ch.ethz.intervals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Assert;
import org.junit.Test;

import ch.ethz.intervals.impl.IntervalImpl;
import ch.ethz.intervals.impl.LockImpl;

public class TestLocks {
	
	final AtomicInteger stamp = new AtomicInteger();
	
	class IdInterval extends IntervalImpl {
		final List<String> list;
		final String id;
		final LockImpl lockImpl;
		
		public IdInterval(@ParentForNew("Parent") Dependency dep, List<String> list, String id, LockImpl l1) {
			super(dep);
			Intervals.addExclusiveLock(this, l1);
			this.lockImpl = l1;
			this.list = list;
			this.id = id;
		}
		
		@Override public String toString() {
			return id;
		}

		@Override protected void run() {
			Assert.assertTrue(Intervals.checkWritable(lockImpl));
			Assert.assertTrue(this.locks(lockImpl));
			list.add(id);
		}
	}
	
	public static final int REPEAT = 300;
	public static final int REPEAT_SMALL = 25; // some tests sleep or otherwise take a long time

	// Just test that locks work at all:
	@Test public void simple() {
		for(int i = 0; i < REPEAT; i++) {
			final LockImpl l1 = new LockImpl();
			final List<String> ids = new ArrayList<String>();
			Intervals.inline(new VoidInlineTask() {			
				@Override public void run(IntervalImpl subinterval) {
					new IdInterval(subinterval, ids, "0", l1);					
					new IdInterval(subinterval, ids, "1", l1);					
					new IdInterval(subinterval, ids, "2", l1);					
				}
			});
			Assert.assertEquals(3, ids.size());
			Assert.assertTrue(ids.contains("0"));
			Assert.assertTrue(ids.contains("1"));
			Assert.assertTrue(ids.contains("2"));
		}
	}
	
	// Test recursive locking:
	@Test public void recursive() {
		for(int i = 0; i < REPEAT; i++) {
			final LockImpl l1 = new LockImpl();
			final List<String> ids = new ArrayList<String>();
			Intervals.inline(new VoidInlineTask() {			
				@Override public void run(IntervalImpl subinterval) {
					IntervalImpl a = new IdInterval(subinterval, ids, "a", l1);
					IntervalImpl a1 = new IdInterval(a, ids, "a1", l1);
					IntervalImpl a11 = new IdInterval(a1, ids, "a11", l1);
					IntervalImpl a111 = new IdInterval(a11, ids, "a111", l1);
					IntervalImpl a2 = new IdInterval(a, ids, "a2", l1);
					new IdInterval(a2, ids, "a21", l1);
				}
			});
			
			Assert.assertEquals(6, ids.size());
			Assert.assertTrue(ids.contains("a"));
			Assert.assertTrue(ids.contains("a1"));
			Assert.assertTrue(ids.contains("a11"));
			Assert.assertTrue(ids.contains("a111"));
			Assert.assertTrue(ids.contains("a2"));
			Assert.assertTrue(ids.contains("a21"));
			
			Assert.assertEquals(0, ids.indexOf("a"));
			Assert.assertEquals(ids.indexOf("a1")+1, ids.indexOf("a11"));
			Assert.assertEquals(ids.indexOf("a11")+1, ids.indexOf("a111"));
			Assert.assertEquals(ids.indexOf("a2")+1, ids.indexOf("a21"));
		}
	}
	
	
	class LockingVoidSubinterval extends VoidInlineTask {
		final String name;
		final long[] times;
		final List<String> ids;
		final LockImpl l1;
		
		public LockingVoidSubinterval(String n, long[] times, List<String> ids, LockImpl l1) {
			this.name = n;
			this.times = times;
			this.ids = ids;
			this.l1 = l1;
		}
		@Override public String toString() {
			return name;
		}
		@Override public void init(IntervalImpl subinterval) {
			Intervals.addExclusiveLock(subinterval, l1);
		}
		@Override public void run(IntervalImpl subinterval) {
			times[0] = System.currentTimeMillis();
			ids.add(name+":"+Intervals.checkWritable(l1));
			try { // just to make it more likely that the lock fails
				Thread.sleep(100);
			} catch (InterruptedException e) {
			}
			times[1] = System.currentTimeMillis();
		}
	}
	
	/** Tests that two subintervals (within different master intervals) contending for a lock
	 *  both eventually get it, but run in disjoint times: */
	@Test public void subinterLock() {
		for(int i = 0; i < REPEAT_SMALL; i++) {
			final LockImpl l1 = new LockImpl();
			final List<String> ids = Collections.synchronizedList(new ArrayList<String>());
			final long[] aTimes = new long[2];
			final long[] bTimes = new long[2];
			
			Intervals.inline(new VoidInlineTask() {			
				@Override public String toString() {
					return "outer";
				}
				@Override public void run(IntervalImpl subinterval) {
					new IntervalImpl(subinterval, "inner1") {						
						@Override protected void run() {
							Intervals.inline(new LockingVoidSubinterval("a", aTimes, ids, l1));
						}
					};
					
					new IntervalImpl(subinterval, "inner2") {						
						@Override protected void run() {
							Intervals.inline(new LockingVoidSubinterval("b", bTimes, ids, l1));
						}
					};
				}
			});
			
			Assert.assertEquals(2, ids.size());
			
			Assert.assertTrue(ids.contains("a:true"));
			Assert.assertTrue(ids.contains("b:true"));
			
			Assert.assertTrue(aTimes[0] <= aTimes[1]);
			Assert.assertTrue(bTimes[0] <= bTimes[1]);
			Assert.assertTrue(
					String.format("aTimes=(%s-%s) bTimes=(%s-%s)", aTimes[0], aTimes[1], bTimes[0], bTimes[1]),
					aTimes[1] <= bTimes[0] || bTimes[1] <= aTimes[0]);
		}
	}	
	
	/** Like {@link #subinterLock()} but the locks are acquired by async intervals: */
	@Test public void interLock() {
		for(int i = 0; i < REPEAT_SMALL; i++) {
			final LockImpl l1 = new LockImpl();
			final List<String> ids = Collections.synchronizedList(new ArrayList<String>());
			final long[] aTimes = new long[2];
			final long[] bTimes = new long[2];
			
			Intervals.inline(new VoidInlineTask() {			
				@Override public String toString() {
					return "outer";
				}
				@Override public void run(IntervalImpl subinterval) {
					IntervalImpl a = new IntervalImpl(subinterval, "a") {						
						@Override protected void run() {
							new LockingVoidSubinterval("a", aTimes, ids, l1).run(this);
						}
					};
					Intervals.addExclusiveLock(a, l1);
					
					IntervalImpl b = new IntervalImpl(subinterval, "b") {						
						@Override protected void run() {
							new LockingVoidSubinterval("b", bTimes, ids, l1).run(this);
						}
					};
					Intervals.addExclusiveLock(b, l1);
				}
			});
			
			Assert.assertEquals(2, ids.size());
			
			Assert.assertTrue(ids.contains("a:true"));
			Assert.assertTrue(ids.contains("b:true"));
			
			Assert.assertTrue(aTimes[0] <= aTimes[1]);
			Assert.assertTrue(bTimes[0] <= bTimes[1]);
			Assert.assertTrue(
					String.format("aTimes=(%s-%s) bTimes=(%s-%s)", aTimes[0], aTimes[1], bTimes[0], bTimes[1]),
					aTimes[1] <= bTimes[0] || bTimes[1] <= aTimes[0]);
		}
	}
	
	// If locks are acquired strictly in order, should not deadlock.
	@Test public void lockOrdering() {
		for(int repeat = 0; repeat < REPEAT; repeat++) {
			final LockImpl l1 = new LockImpl(), l2 = new LockImpl();
			final int[] executed = new int[3];
			
			Intervals.inline(new VoidInlineTask() {			
				@Override public String toString() {
					return "outer";
				}
				@Override public void run(IntervalImpl subinterval) {
					IntervalImpl a1 = new IntervalImpl(subinterval, "a1") {						
						@Override protected void run() {
							IntervalImpl a2 = new IntervalImpl(this, "a2") {
								@Override protected void run() { executed[1]++; }
							};
							Intervals.addExclusiveLock(a2, l2);
							executed[0]++;
						}
					};
					Intervals.addExclusiveLock(a1, l1);
	
					IntervalImpl b = new IntervalImpl(subinterval, "b") {						
						@Override protected void run() {
							executed[2]++;
						}
					};
					
					Intervals.addHb(a1.start, b.start);
					Intervals.addExclusiveLock(b, l1);
					Intervals.addExclusiveLock(b, l2);
				}
			});
			
			Assert.assertEquals(1, executed[0]);
			Assert.assertEquals(1, executed[1]);
			Assert.assertEquals(1, executed[2]);
		}
	}
		
	/** Same test as {@link #lockOrdering()} but {@code b} is a subinterval */
	@Test public void lockOrderingSubinter() {
		for(int repeat = 0; repeat < REPEAT; repeat++) {
			final LockImpl l1 = new LockImpl(), l2 = new LockImpl();
			final int[] executed = new int[3];
			
			Intervals.inline(new VoidInlineTask() {			
				@Override public String toString() {
					return "outer";
				}
				@Override public void run(IntervalImpl subinterval) {
					IntervalImpl a1 = new IntervalImpl(subinterval, "a1") {						
						@Override protected void run() {
							IntervalImpl a2 = new IntervalImpl(this, "a2") {
								@Override protected void run() { executed[1]++; }
							};
							Intervals.addExclusiveLock(a2, l2);
							executed[0]++;
						}
					};
					Intervals.addExclusiveLock(a1, l1);
	
					IntervalImpl bOuter = new IntervalImpl(subinterval, "bOuter") {						
						@Override protected void run() {
							Intervals.inline(new VoidInlineTask() {
								@Override public String toString() { return "b"; }
								@Override public void init(IntervalImpl subinterval) {
									Intervals.addExclusiveLock(subinterval, l1);
									Intervals.addExclusiveLock(subinterval, l2);
								}
								@Override public void run(IntervalImpl subinterval) {
									executed[2]++;
								}
							});
						}
					};					
					Intervals.addHb(a1.start, bOuter.start);
				}
			});
			
			Assert.assertEquals(1, executed[0]);
			Assert.assertEquals(1, executed[1]);
			Assert.assertEquals(1, executed[2]);
		}
	}

}
