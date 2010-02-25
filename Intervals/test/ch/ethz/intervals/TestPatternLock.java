package ch.ethz.intervals;

import org.junit.Test;

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
			
		final Lock l = new Lock();
		
		Intervals.inline(new VoidInlineTask() {
			public void run(Interval subinterval) {
				new SetupInterval(subinterval) {
					@Override protected void setup(Point setupEnd, Interval worker) {
						Interval a = new DebugInterval(worker, ("a"));
						Interval b = new DebugInterval(worker, ("b"));
						Interval c = new DebugInterval(worker, ("c"));
						Interval b1 = new DebugInterval(b, ("b1"));
						Interval b11 = new DebugInterval(b, ("b11"));
						Interval b2 = new DebugInterval(b, ("b2"));
						
						Intervals.addHb(a.end, b.start);
						Intervals.addHb(b.end, c.start);
						Intervals.addHb(b1.end, b11.start);
						
						Interval l1 = (Interval) new DebugInterval(b, ("l1"));
						Interval l2 = (Interval) new DebugInterval(b, ("l2"));
						
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
