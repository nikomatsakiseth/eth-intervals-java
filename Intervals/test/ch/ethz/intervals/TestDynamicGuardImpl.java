package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.emptyTask;
import static ch.ethz.intervals.Intervals.end;
import static ch.ethz.intervals.Intervals.intervalDuring;
import static ch.ethz.intervals.Intervals.start;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

public class TestDynamicGuardImpl {
	
	IntervalImpl a;
	IntervalImpl b;
	IntervalImpl b1, b11;
	IntervalImpl b2;
	IntervalImpl c;
	
	IntervalImpl l1, l2;
	
	DynamicGuardImpl dg;
	
	public static Task debugTask(String name) {
		return new Intervals.NamedTask(name) {
			public void run(Point currentEnd) {
				System.out.printf("%s: currentEnd=%s\n", name, currentEnd);
			}
		};
	};
	
	
	@Before public void before() {
		
		/* a -> b------------------> c
		 *      |                  ^
		 *      |--> b1 -> b11 ----|
		 *      |--> b2 -----------|
		 *      |--> l1 -----------|
		 *      |--> l2 -----------|
		 */
		
		dg = (DynamicGuardImpl) Guards.newDynamicGuard();
		
		Intervals.blockingInterval(new SetupTask() {
			@Override
			public void setup(Point currentEnd, Interval worker) {
				System.out.printf("end of setup = %s worker = %s\n", currentEnd, worker);
				a = (IntervalImpl) intervalDuring(worker).schedule(debugTask("a"));
				b = (IntervalImpl) intervalDuring(worker).startAfter(end((a))).schedule(debugTask("b"));
				c = (IntervalImpl) intervalDuring(worker).startAfter(end((b))).schedule(debugTask("c"));
				b1 = (IntervalImpl) intervalDuring(b).schedule(debugTask("b1"));
				b11 = (IntervalImpl) intervalDuring(b).startAfter(end((b1))).schedule(debugTask("b11"));
				b2 = (IntervalImpl) intervalDuring(b).schedule(debugTask("b2"));
				
				l1 = (IntervalImpl) intervalDuring(b).exclusiveLock(dg).schedule(debugTask("l1"));
				l2 = (IntervalImpl) intervalDuring(b).exclusiveLock(dg).schedule(debugTask("l2"));
			}
		});
				
	}
	
	@Test
	public void justWrite() {
		dg.checkWrite(b);		
	}
	
	@Test
	public void justRead() {
		dg.checkRead(c);		
	}
	
	@Test(expected=DataRaceException.class) 
	public void writeInParentReadInChildren() {
		dg.checkWrite(b);
		dg.checkRead(b1);
	}

	@Test
	public void writeBeforeParentReadInChildren() {
		dg.checkWrite(a);
		dg.checkRead(b1);
	}
	
	@Test
	public void writeBeforeParentReadInMultipleChildren() {
		dg.checkWrite(a);
		dg.checkRead(b1);
		dg.checkRead(b2);
	}
	
	@Test
	public void writeBeforeParentReadInMultipleChildrenAndFinallyToUncle() {
		dg.checkWrite(a);
		dg.checkRead(b1);
		dg.checkRead(b2);
		dg.checkWrite(c);
	}
	
	@Test
	public void writeBeforeParentReadAndWriteInChildren() {
		dg.checkWrite(a);
		dg.checkRead(b1);
		dg.checkWrite(b11);
	}
	
	@Test(expected=DataRaceException.class)
	public void writeBeforeParentReadAndWriteInMultipleChildren() {
		dg.checkWrite(a);
		dg.checkRead(b1);
		dg.checkRead(b2);
		dg.checkWrite(b11);
	}
	
//	@Test(expected=DataRaceException.class)
//	public void unorderedSharedLock() {
//		// Actually, in the future this should work!  Right now the dynamic
//		// check does not distinguish a SHARED lock, which ought to proceed
//		// in parallel with a READ.
//		Mockito.when(b1.holdsLockOn(dg, false)).thenReturn(true);
//		dg.checkRead(b1);
//		dg.checkRead(b2);
//	}
	
	@Test(expected=DataRaceException.class)
	public void unorderedExclusiveLockAndUnlock1() {
		dg.checkWrite(l1);
		dg.checkWrite(b1);
	}	
	
	@Test(expected=DataRaceException.class)
	public void unorderedExclusiveLockAndUnlock2() {
		dg.checkWrite(b1);
		dg.checkWrite(l1);
	}	
	
	@Test(expected=DataRaceException.class)
	public void unorderedExclusiveLockAndUnlock3() {
		dg.checkWrite(l2);
		dg.checkWrite(b1);
	}	
	
	@Test
	public void orderedExclusiveLockAndUnlock() {
		dg.checkWrite(l1);		
		dg.checkWrite(c);
	}	
	
	/** Why, you might wonder, does this test results in a {@link DataRaceException}?
	 *  Because the locks are acquired in a specific (implementation-dependent!) order, 
	 *  and the checkWrite() calls only work in that order.  So, in our
	 *  impl., l2 gets awakened first, acquires the lock on dg,
	 *  and so l2.end->l1.start.  Therefore, it is not safe (nor possible)
	 *  for l1 to write before l2.  If the impl. were changed so
	 *  that l1 got the lock first, then {@link #twoUnorderedExclusiveLock2()}
	 *  would result in a {@link DataRaceException} */
	@Test(expected=DataRaceException.class)
	public void twoUnorderedExclusiveLock1() {
		dg.checkWrite(l2);
		dg.checkWrite(l1);
	}
	
	@Test
	public void twoUnorderedExclusiveLock2() {
		dg.checkWrite(l1);
		dg.checkWrite(l2);
	}
	
}
