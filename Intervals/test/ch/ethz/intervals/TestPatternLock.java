package ch.ethz.intervals;

import org.junit.Test;

import ch.ethz.intervals.impl.IntervalImpl;
import ch.ethz.intervals.impl.LockImpl;
import ch.ethz.intervals.impl.PointImpl;

public class TestPatternLock {
	
	public static class DebugInterval extends EmptyInterval {

		public DebugInterval(@ParentForNew("Parent") Dependency dep, String name) {
			super(dep, name);
		}
		
		@Override
		public void run() {
			System.err.printf("Executing: %s (%s-%s)\n", this, start, end);
		}
		
	}
	
	@Test
	public void test() {
		// This pattern often exposes problems,
		// generally due to the two intervals acquiring
		// the same lock 'l':
		
		/* a -> b------------------> c
		 *      |                  ^
		 *      |--> b1 -> b11 ----|
		 *      |--> b2 -----------|
		 *      |--> l1 -----------|
		 *      |--> l2 -----------|
		 */
			
		final LockImpl l = new LockImpl();
		
		Intervals.inline(new VoidInlineTask() {
			public void run(IntervalImpl subinterval) {
				new SetupInterval(subinterval) {
					@Override protected void setup(PointImpl setupEnd, IntervalImpl worker) {
						IntervalImpl a = new DebugInterval(worker, ("a"));
						IntervalImpl b = new DebugInterval(worker, ("b"));
						IntervalImpl c = new DebugInterval(worker, ("c"));
						IntervalImpl b1 = new DebugInterval(b, ("b1"));
						IntervalImpl b11 = new DebugInterval(b, ("b11"));
						IntervalImpl b2 = new DebugInterval(b, ("b2"));
						
						Intervals.addHb(a.end, b.start);
						Intervals.addHb(b.end, c.start);
						Intervals.addHb(b1.end, b11.start);
						
						IntervalImpl l1 = (IntervalImpl) new DebugInterval(b, ("l1"));
						IntervalImpl l2 = (IntervalImpl) new DebugInterval(b, ("l2"));
						
						Intervals.addExclusiveLock(l1, l);
						Intervals.addExclusiveLock(l2, l);
						
						System.out.printf("a=%s b=%s c=%s b1=%s b11=%s b2=%s l1=%s l2=%s setup.end=%s, worker=%s\n", 
								a, b, c, b1, b11, b2, l1, l2, setupEnd, worker);					
					}					
				};
			}
		});				
	}
}
