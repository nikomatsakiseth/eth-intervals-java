package ch.ethz.intervals.impl;

import org.junit.Test;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.impl.IntervalImpl;
import ch.ethz.intervals.impl.LockImpl;
import ch.ethz.intervals.impl.PointImpl;
import ch.ethz.intervals.task.AbstractTask;
import ch.ethz.intervals.task.SetupTask;

public class TestPatternLock {
	
	public static class DebugInterval extends AbstractTask {

		public DebugInterval(String name) {
			super(name);
		}
		
		@Override
		public void run(Interval current) {
			System.err.printf("Executing: %s (%s-%s)\n", this, current.getStart(), current.getEnd());
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
		
		Intervals.inline(new SetupTask("outer") {
			private Interval debugInterval(Interval worker, String name) {
				return worker.newAsyncChild(new DebugInterval(name));
			}
			public void setup(Interval setup, Interval worker) {
				Interval a = debugInterval(worker, ("a"));
				Interval b = debugInterval(worker, ("b"));
				Interval c = debugInterval(worker, ("c"));
				Interval b1 = debugInterval(b, ("b1"));
				Interval b11 = debugInterval(b, ("b11"));
				Interval b2 = debugInterval(b, ("b2"));
				
				Intervals.addHb(a, b);
				Intervals.addHb(b, c);
				Intervals.addHb(b1, b11);
				
				Interval l1 = debugInterval(b, ("l1"));
				Interval l2 = debugInterval(b, ("l2"));
				
				l1.addLock(l);
				l2.addLock(l);
				
				System.out.printf("a=%s b=%s c=%s b1=%s b11=%s b2=%s l1=%s l2=%s setup=%s, worker=%s\n", 
						a, b, c, b1, b11, b2, l1, l2, setup, worker);					
			}					
		});				
	}
}
