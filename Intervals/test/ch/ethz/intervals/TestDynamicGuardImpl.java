package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.intervalDuring;

import org.junit.Before;
import org.junit.Test;

public class TestDynamicGuardImpl {
	
	Interval a;
	Interval b;
	Interval b1, b11;
	Interval b2;
	Interval c;
	
	Interval l1, l2;
	
	DynamicGuardImpl dg;
	
	public static Task debugTask(String name) {
		return new NamedTask(name) {
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
				a = (Interval) intervalDuring(worker, debugTask("a"));
				b = (Interval) intervalDuring(worker, debugTask("b"));
				c = (Interval) intervalDuring(worker, debugTask("c"));
				b1 = (Interval) intervalDuring(b, debugTask("b1"));
				b11 = (Interval) intervalDuring(b, debugTask("b11"));
				b2 = (Interval) intervalDuring(b, debugTask("b2"));
				
				Intervals.addHb(a.end(), b.start());
				Intervals.addHb(b.end(), c.start());
				Intervals.addHb(b1.end(), b11.start());
				
				l1 = (Interval) intervalDuring(b, debugTask("l1"));
				l2 = (Interval) intervalDuring(b, debugTask("l2"));
				
				Intervals.exclusiveLock(l1, dg);
				Intervals.exclusiveLock(l2, dg);
				
				System.out.printf("a=%s b=%s c=%s b1=%s b11=%s b2=%s l1=%s l2=%s setup.end=%s, worker=%s\n", 
						a, b, c, b1, b11, b2, l1, l2, currentEnd, worker);
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
