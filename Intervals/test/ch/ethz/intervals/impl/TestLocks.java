package ch.ethz.intervals.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Assert;
import org.junit.Test;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.task.AbstractTask;

public class TestLocks {
	
	final AtomicInteger stamp = new AtomicInteger();
	
	class IdInterval extends AbstractTask {
		final List<String> list;
		final String id;
		final LockImpl lock;
		
		public IdInterval(List<String> list, String id, LockImpl l1) {
			super(id);
			this.lock = l1;
			this.list = list;
			this.id = id;
		}
		
		@Override
		public void attachedTo(Interval inter) {
			super.attachedTo(inter);
			inter.addLock(lock);
		}

		@Override 
		public void run(Interval current) {
			Assert.assertTrue(Intervals.checkWritable(lock));
			Assert.assertTrue(current.locks(lock));
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
			Intervals.inline(new AbstractTask("iter"+i) {			
				@Override public void run(Interval subinterval) {
					subinterval.newAsyncChild(new IdInterval(ids, "0", l1));					
					subinterval.newAsyncChild(new IdInterval(ids, "1", l1));					
					subinterval.newAsyncChild(new IdInterval(ids, "2", l1));					
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
			Intervals.inline(new AbstractTask("iter"+i) {			
				@Override public void run(Interval subinterval) {
					Interval a = subinterval.newAsyncChild(new IdInterval(ids, "a", l1));
					Interval a1 = a.newAsyncChild(new IdInterval(ids, "a1", l1));
					Interval a11 = a1.newAsyncChild(new IdInterval(ids, "a11", l1));
					Interval a111 = a11.newAsyncChild(new IdInterval(ids, "a111", l1));
					Interval a2 = a.newAsyncChild(new IdInterval(ids, "a2", l1));
					a2.newAsyncChild(new IdInterval(ids, "a21", l1));
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
	
	
	class LockingVoidSubinterval extends AbstractTask {
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
		@Override public void attachedTo(Interval subinterval) {
			subinterval.addLock(l1);
		}
		@Override public void run(Interval subinterval) {
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
			
			Intervals.inline(new AbstractTask() {			
				@Override public String toString() {
					return "outer";
				}
				@Override public void run(Interval subinterval) {
					subinterval.newAsyncChild(new AbstractTask("inner1") {
						@Override public void run(Interval current) throws Exception {
							Intervals.inline(new LockingVoidSubinterval("a", aTimes, ids, l1));
						}
					});
					
					subinterval.newAsyncChild(new AbstractTask("inner2") {
						@Override public void run(Interval current) throws Exception {
							Intervals.inline(new LockingVoidSubinterval("b", bTimes, ids, l1));
						}
					});
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
			
			Intervals.inline(new AbstractTask() {			
				@Override public String toString() {
					return "outer";
				}
				@Override public void run(Interval subinterval) {
					Interval a = subinterval.newAsyncChild(new AbstractTask("a") {
						@Override public void run(Interval current) throws Exception {
							new LockingVoidSubinterval("a", aTimes, ids, l1).run(current);
						}
					}); 
					a.addLock(l1);
					
					Interval b = subinterval.newAsyncChild(new AbstractTask("a") {
						@Override public void run(Interval current) throws Exception {
							new LockingVoidSubinterval("b", bTimes, ids, l1).run(current);
						}
					});
					b.addLock(l1);
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
			
			Intervals.inline(new AbstractTask("outer") {			
				@Override public void run(Interval subinterval) {
					Interval a1 = subinterval.newAsyncChild(new AbstractTask("a1") {
						@Override public void run(Interval current) {
							Interval a2 = current.newAsyncChild(new AbstractTask("a2") {
								@Override public void run(Interval current) {
									executed[1]++; 
								}
							});
							a2.addLock(l2);
							executed[0]++;
						}
					});
					a1.addLock(l1);
	
					Interval b = subinterval.newAsyncChild(new AbstractTask("b") {
						@Override public void run(Interval current) {
							executed[2]++;
						}
					});
					
					Intervals.addHb(a1.getStart(), b.getStart());
					b.addLock(l1);
					b.addLock(l2);
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
			
			Intervals.inline(new AbstractTask("outer") {
				@Override public void run(Interval subinterval) {
					Interval a1 = subinterval.newAsyncChild(new AbstractTask("a1") {
						@Override public void run(Interval current) {
							Interval a2 = current.newAsyncChild(new AbstractTask("a2") {
								@Override public void run(Interval current) {
									executed[1]++;
								}
							});
							a2.addLock(l2);
							executed[0]++;
						}
					});
					a1.addLock(l1);
	
					Interval bOuter = subinterval.newAsyncChild(new AbstractTask("bOuter") {
						@Override public void run(Interval current) {
							Intervals.inline(new AbstractTask("b") {
								@Override public void attachedTo(Interval subinterval) {
									subinterval.addLock(l1);
									subinterval.addLock(l2);
								}
								@Override public void run(Interval subinterval) {
									executed[2]++;
								}
							});
						}
					}); 
					Intervals.addHb(a1.getStart(), bOuter.getStart());
				}
			});
			
			Assert.assertEquals(1, executed[0]);
			Assert.assertEquals(1, executed[1]);
			Assert.assertEquals(1, executed[2]);
		}
	}

}
