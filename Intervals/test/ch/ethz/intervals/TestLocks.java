package ch.ethz.intervals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Assert;
import org.junit.Test;

public class TestLocks {
	
	final AtomicInteger stamp = new AtomicInteger();
	
	class IdInterval extends Interval {
		final List<String> list;
		final String id;
		final Lock lock;
		
		public IdInterval(Dependency dep, List<String> list, String id, Lock l1) {
			super(dep);
			Intervals.addExclusiveLock(this, l1);
			this.lock = l1;
			this.list = list;
			this.id = id;
		}
		
		@Override public String toString() {
			return id;
		}

		@Override protected void run() {
			assert lock.isWritable();
			assert this.holdsLock(lock);
			list.add(id);
		}
	}
	
	public static final int REPEAT = 300;
	
	// Just test that locks work at all:
	@Test public void simple() {
		for(int i = 0; i < REPEAT; i++) {
			final Lock l1 = new Lock();
			final List<String> ids = new ArrayList<String>();
			Intervals.subinterval(new VoidSubinterval() {			
				@Override public void run(Interval subinterval) {
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
			final Lock l1 = new Lock();
			final List<String> ids = new ArrayList<String>();
			Intervals.subinterval(new VoidSubinterval() {			
				@Override public void run(Interval subinterval) {
					Interval a = new IdInterval(subinterval, ids, "a", l1);
					Interval a1 = new IdInterval(a, ids, "a1", l1);
					Interval a11 = new IdInterval(a1, ids, "a11", l1);
					Interval a111 = new IdInterval(a11, ids, "a111", l1);
					Interval a2 = new IdInterval(a, ids, "a2", l1);
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
	
	
	// Test subinterval locking:
	@Test public void subinterLock() {
		for(int i = 0; i < 50; i++) {
			final Lock l1 = new Lock();
			final List<String> ids = Collections.synchronizedList(new ArrayList<String>());
			final long[] aTimes = new long[2];
			final long[] bTimes = new long[2];
			
			class LockingVoidSubinterval extends VoidSubinterval {
				final String name;
				final long[] times;
				public LockingVoidSubinterval(String n, long[] times) {
					this.name = n;
					this.times = times;
				}
				@Override public String toString() {
					return name;
				}
				@Override public Lock[] locks() {
					return new Lock[] { l1 };
				}
				@Override public void run(Interval subinterval) {
					times[0] = System.currentTimeMillis();
					ids.add(name+":"+l1.isWritable());
					try { // just to make it more likely that the lock fails
						Thread.sleep(100);
					} catch (InterruptedException e) {
					}
					times[1] = System.currentTimeMillis();
				}
			}
			
			Intervals.subinterval(new VoidSubinterval() {			
				@Override public String toString() {
					return "outer";
				}
				@Override public void run(Interval subinterval) {
					new Interval(subinterval, "inner1") {						
						@Override protected void run() {
							Intervals.subinterval(new LockingVoidSubinterval("a", aTimes));
						}
					};
					
					new Interval(subinterval, "inner2") {						
						@Override protected void run() {
							Intervals.subinterval(new LockingVoidSubinterval("b", bTimes));
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
	
}
