package ch.ethz.intervals.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Assert;
import org.junit.Test;

import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.mirror.Interval;
import ch.ethz.intervals.task.AbstractTask;
import ch.ethz.intervals.task.EmptyTask;

public class TestInterval {
	
	/** many tests are not guaranteed to fail, so we repeat them 
	 *  many times to stress the scheduler... not the best solution! */
	final int repeat = 1000;
	
	protected static void debug(String fmt, Object... args) {
		//System.err.println(String.format(fmt, args));
	}
	
	public static class IncTask extends AbstractTask {
		public final AtomicInteger i;
		public final int amnt;

		public IncTask(String name, AtomicInteger i) {
			this(name, i, 1);
		}

		public IncTask(String name, AtomicInteger i, int amnt) {
			super(name);
			this.i = i;
			this.amnt = amnt;
		}

		@Override
		public void run(Interval current) {
			i.addAndGet(amnt);
		}
	}
	
	class AddTask extends AbstractTask {

		public final List<List<Integer>> list;
		public final List<Integer> id;
		
		public AddTask(List<List<Integer>> list, Integer... ids) {
			super("Add("+Arrays.asList(ids)+")");
			this.id = Arrays.asList(ids);
			this.list = list;
		}

		@Override
		public void run(Interval current) 
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
			Intervals.inline(new AbstractTask("parentInterval") {
				public void run(final Interval parentInterval) {
					Intervals.inline(new AbstractTask("childInterval") {
						public void run(final Interval childInterval) {							
							Interval after = parentInterval.newAsyncChild(new AddTask(list, 2));
							Intervals.addHb(childInterval, after);
							childInterval.newAsyncChild(new AddTask(list, 1));
							childInterval.newAsyncChild(new AddTask(list, 1));
							childInterval.newAsyncChild(new AddTask(list, 1));							
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
		Intervals.inline(new AbstractTask() {
			public void run(Interval current) {
				for(int i = 0; i < c; i++)
					current.newAsyncChild(new IncTask("c"+i, cnt));
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
		Intervals.inline(new AbstractTask() {
			public void run(Interval current) {
				for(int i = 0; i < c; i++) {
					current.newAsyncChild(new IncTask("c"+i, cnt)).schedule();
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
		Intervals.inline(new AbstractTask() {
			public void run(Interval current) {
				Interval future = current.newAsyncChild(new EmptyTask("during"));
				for(int i = 0; i < c; i++)
					future.newAsyncChild(new IncTask("c"+i, cnt));
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
		Intervals.inline(new AbstractTask() {
			public void run(Interval subinterval) {
				Interval worker = subinterval.newAsyncChild(new EmptyTask("worker"));
				Interval d = worker.newAsyncChild(new EmptyTask("d"));
				Interval k = d.newAsyncChild(new EmptyTask("k"));
				Interval n = d.newAsyncChild(new EmptyTask("n"));
				Interval a = worker.newAsyncChild(new EmptyTask("a"));
				Interval m = a.newAsyncChild(new EmptyTask("m"));
				Interval t = a.newAsyncChild(new EmptyTask("t"));
				Interval s = t.newAsyncChild(new EmptyTask("s"));
				
				Assert.assertEquals(d.getEnd(), d.getEnd().mutualBound(d.getEnd()));
				Assert.assertEquals(d.getEnd(), k.getEnd().mutualBound(n.getEnd()));
				Assert.assertEquals(worker.getEnd(), k.getEnd().mutualBound(s.getEnd()));
				Assert.assertEquals(worker.getEnd(), s.getEnd().mutualBound(k.getEnd()));
				Assert.assertEquals(a.getEnd(), m.getEnd().mutualBound(s.getEnd()));
				Assert.assertEquals(a.getEnd(), s.getEnd().mutualBound(m.getEnd()));
				
				successful.addAndGet(1); 
			}
		});
		Assert.assertEquals(successful.get(), 1); // just in case an exc. gets swallowed
	}
}
