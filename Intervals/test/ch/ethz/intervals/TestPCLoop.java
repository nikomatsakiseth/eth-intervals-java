package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.subinterval;
import static ch.ethz.intervals.Intervals.child;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Test;

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
	
	class Consumer extends Interval {
		public Integer i;
		
		public Consumer(Dependency dep, Point endOfPrevConsumer, Integer i) {			
			super(dep);
			Intervals.addHb(endOfPrevConsumer, start);
			this.i = i;
		}

		public void run() {
			consumed.add(i);
		}
	}
	
	@Test public void test() {
		final int MAX = 100;
		subinterval(new VoidSubinterval() {
			public void run(Interval subinterval) {
				Point endOfPrevConsumer = null;
				for(int i = 0; i < MAX; i++) {
					endOfPrevConsumer = new Consumer(child(), endOfPrevConsumer, i).end;
				}
			}
		});
		for(int i = 0; i < MAX; i++)
			assertEquals(i, consumed.get(i).intValue());
	}
	
}
