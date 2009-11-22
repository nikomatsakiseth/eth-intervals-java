package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.end;
import static ch.ethz.intervals.Intervals.blockingInterval;
import static ch.ethz.intervals.Intervals.intervalDuring;
import static ch.ethz.intervals.Intervals.intervalWithBound;
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
public class TestPC {
	
	static AtomicInteger stamp = new AtomicInteger(1);
	public int stamp() {
		return stamp.getAndIncrement();
	}
	
	List<Integer> consumed = new ArrayList<Integer>();
	
	class Consumer implements Task<Void> {
		public Integer i;
		
		private Consumer(Integer i) {
			this.i = i;
		}

		public Void run(Interval<Void> current) {
			consumed.add(i);
			return null;
		}
	}
	
	@Test public void test() {
		final int MAX = 100;
		blockingInterval(new Task<Void>() {
			public Void run(Interval<Void> current) {				
				Point endOfPrevConsumer = null;
				for(int i = 0; i < MAX; i++)
					endOfPrevConsumer =
						intervalWithBound(current.end())
						.startAfter(endOfPrevConsumer)
						.schedule(new Consumer(i))
						.end();
				return null;
			}			
		});
		for(int i = 0; i < MAX; i++)
			assertEquals(i, consumed.get(i).intValue());
	}
	
}
