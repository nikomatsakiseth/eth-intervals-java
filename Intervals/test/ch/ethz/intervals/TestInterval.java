package ch.ethz.intervals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Assert;
import org.junit.Test;

import ch.ethz.intervals.impl.IntervalImpl;

public class TestInterval {
	
	/** many tests are not guaranteed to fail, so we repeat them 
	 *  many times to stress the scheduler... not the best solution! */
	final int repeat = 1000;
	
	protected static void debug(String fmt, Object... args) {
		//System.err.println(String.format(fmt, args));
	}
	
	static class IncTask extends IntervalImpl {
		public final AtomicInteger i;
		public final int amnt;

		public IncTask(@ParentForNew("Parent") Dependency dep, String name, AtomicInteger i) {
			super(dep, name);
			this.i = i;
			this.amnt = 1;
		}

		public IncTask(@ParentForNew("Parent") Dependency dep, String name, AtomicInteger i, int amnt) {
			super(dep, name);
			this.i = i;
			this.amnt = amnt;
		}

		@Override
		public void run() {
			i.addAndGet(amnt);
		}		
	}
	
	class AddTask extends IntervalImpl {

		public final List<List<Integer>> list;
		public final List<Integer> id;
		
		public AddTask(@ParentForNew("Parent") Dependency dep, List<List<Integer>> list, Integer... ids) {
			super(dep, "Add("+Arrays.asList(ids)+")");
			this.id = Arrays.asList(ids);
			this.list = list;
		}

		@Override
		public void run() 
		{
			debug("%s", toString());
			list.add(id);
		}
		
	}
	
	public void checkOrdering(List<List<Integer>> results) {
		List<Integer> current = Arrays.asList(0);
		for (List<Integer> next : results) {
			for (int i = 0; i < current.size(); i++) {
				Assert.assertTrue("Invalid ordering detected: " + current + "/"
						+ next + " in " + results, current.get(i) <= next.get(i));
			}
		}
	}

	@Test public void basic() {
		for (int i = 0; i < repeat; i++) {
			final List<List<Integer>> list = Collections.synchronizedList(new ArrayList<List<Integer>>());
			Intervals.inline(new VoidInlineTask() {
				@Override public String toString() { return "parentInterval"; }
				public void run(final IntervalImpl parentInterval) {
					Intervals.inline(new VoidInlineTask() {
						@Override public String toString() { return "childInterval"; }
						public void run(final IntervalImpl childInterval) {							
							IntervalImpl after = new AddTask(parentInterval, list, 2);
							Intervals.addHb(childInterval.end, after.start);
							new AddTask(childInterval, list, 1);
							new AddTask(childInterval, list, 1);
							new AddTask(childInterval, list, 1);							
						}
					});					
				}
			});
			Assert.assertEquals("equal", 4, list.size());
			checkOrdering(list);
		}
	}
	
	/**
	 * Check that if we have a whole bunch of children, they all
	 * complete and execute once.  Don't schedule them right away
	 * to stress test unscheduled list as it gets bigger.
	 */
	@Test public void manyChildren() {
		final int c = 1024;
		final AtomicInteger cnt = new AtomicInteger();
		Intervals.inline(new VoidInlineTask() {
			public void run(IntervalImpl _) {
				for(int i = 0; i < c; i++)
					new IncTask(Intervals.child(), "c"+i, cnt);
			}			
		});
		Assert.assertEquals(cnt.get(), c);
	}
	
	/**
	 * Check that if we have a whole bunch of children, they all
	 * complete and execute once.  Schedule them right away.
	 */
	@Test public void manyChildrenScheduled() {
		final int c = 1024;
		final AtomicInteger cnt = new AtomicInteger();
		Intervals.inline(new VoidInlineTask() {
			public void run(IntervalImpl _) {
				for(int i = 0; i < c; i++) {
					new IncTask(Intervals.child(), "c"+i, cnt).schedule();
				}
			}
			
		});
		Assert.assertEquals(cnt.get(), c);
	}
	
	/**
	 * Check that if we schedule a bunch of tasks as children of
	 * a future, not-yet-scheduled interval, everything still works.
	 */
	@Test public void manyDuring() {
		final int c = 1024;
		final AtomicInteger cnt = new AtomicInteger();
		Intervals.inline(new VoidInlineTask() {
			public void run(IntervalImpl _) {
				IntervalImpl future = new EmptyInterval(Intervals.child(), "during");
				for(int i = 0; i < c; i++)
					new IncTask(future, "c"+i, cnt);
			}
			
		});
		Assert.assertEquals(cnt.get(), c);
	}
	
	@Test public void ancestor() {
		/*                    worker
		 *                   /      \
		 *                  d        a
		 *                 / \      / \
		 *                k   n    m   t
		 *                              \
		 *                               s
		 */
		final AtomicInteger successful = new AtomicInteger();
		Intervals.inline(new VoidInlineTask() {
			public void run(IntervalImpl subinterval) {
				IntervalImpl worker = new EmptyInterval(subinterval, "worker");
				IntervalImpl d = new EmptyInterval(worker, "d");
				IntervalImpl k = new EmptyInterval(d, "k");
				IntervalImpl n = new EmptyInterval(d, "n");
				IntervalImpl a = new EmptyInterval(worker, "a");
				IntervalImpl m = new EmptyInterval(a, "m");
				IntervalImpl t = new EmptyInterval(a, "t");
				IntervalImpl s = new EmptyInterval(t, "s");
				
				Assert.assertEquals(d.end, d.end.mutualBound(d.end));
				Assert.assertEquals(d.end, k.end.mutualBound(n.end));
				Assert.assertEquals(worker.end, k.end.mutualBound(s.end));
				Assert.assertEquals(worker.end, s.end.mutualBound(k.end));
				Assert.assertEquals(a.end, m.end.mutualBound(s.end));
				Assert.assertEquals(a.end, s.end.mutualBound(m.end));
				
				successful.addAndGet(1); 
			}
		});
		Assert.assertEquals(successful.get(), 1); // just in case an exc. gets swallowed
	}
}
