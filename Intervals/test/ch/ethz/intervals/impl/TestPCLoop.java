package ch.ethz.intervals.impl;

import static ch.ethz.intervals.Intervals.inline;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Test;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.task.AbstractTask;

/**
 * Simple Producer-Consumer Example 
 *
 * Each instance of the Consumer class consumes a single 
 * data item.  The method test represents the producer,
 * which creates one Consumer for each data item, 
 * and arranges that the consumers always consume each item 
 * in the order produced.
 */
public class TestPCLoop {
	
	static AtomicInteger stamp = new AtomicInteger(1);
	public int stamp() {
		return stamp.getAndIncrement();
	}
	
	List<Integer> consumed = new ArrayList<Integer>();
	
	class Consumer extends AbstractTask {
		public Integer i;
		
		public Consumer(Integer i) {			
			this.i = i;
		}

		public void run(Interval _) {
			consumed.add(i);
		}
	}
	
	@Test public void test() {
		final int MAX = 100;
		inline(new AbstractTask() {
			public void run(Interval subinterval) {
				Interval prevConsumer = null;
				for(int i = 0; i < MAX; i++) {
					Interval consumer = subinterval.newAsyncChild(new Consumer(i));
					Intervals.addHbIfNotNull(prevConsumer, consumer);
					prevConsumer = consumer;
				}
			}
		});
		for(int i = 0; i < MAX; i++)
			assertEquals(i, consumed.get(i).intValue());
	}
	
}
