package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.blockingInterval;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

/**
 * Agent Producer-Consumer Example 
 *
 * Similar to {@link TestPC}, except that each consumer
 * creates the next consumer, and each producer creates the
 * next producer. 
 * 
 * We use interval return values so that the
 * {@link Producer} simply returns a {@link ProducerData} instance
 * which contains both the produced data and the interval for the
 * next producer.  The {@link Consumer} reads this result, consumes
 * the generated data, and creates the next consumer interval (which
 * must come after the next producer has completed).  In this way,
 * the interval return value serves as an abstraction barrier between the
 * {@link Producer} and {@link Consumer}, so that the implementation of
 * either could be changed with affecting the other.
 */
public class TestPCAgent {
	
	static int nextStamp;
	public synchronized int stamp() {
		return nextStamp++;
	}
	
	final int MAX = 100;
	List<Integer> consumed = new ArrayList<Integer>();
	
	/** Where the producer writes its data.  There is one 
	 *  instance per producer. It is not safe to read these
	 *  fields unless the end of the producer <em>happens before</em>
	 *  the interval which is reading. */
	class ProducerData {
		/** Data producer by producer <em>i</em> */
		public int produced;
		
		/** End of producer <em>i+1</em> */
		public Point endOfNextProducer;
		
		/** Where producer <em>i+1</em> will write its data */
		public ProducerData dataForNextProducer;		
	}
	
	class Producer extends AbstractTask {
		private final ProducerData data;
		private final int index;
		
		public Producer(ProducerData producerData, int index) {
			this.data = producerData;
			this.index = index;
		}

		public void run(Point currentEnd) {
			data.produced = index;
			
			if(index + 1 < MAX) { // Create next producer, if any.
				data.dataForNextProducer = new ProducerData();
				Producer nextProducer = new Producer(data.dataForNextProducer, index + 1);
				data.endOfNextProducer = 
					Intervals.intervalWithBound(currentEnd.bound())
						.startAfter(currentEnd)
						.schedule(nextProducer)
						.end();
			} 
		}
	}
	
	class Consumer extends AbstractTask {
		private final ProducerData data;

		public Consumer(ProducerData data) {
			this.data = data;
		}

		public void run(Point currentEnd) {
			consumed.add(data.produced); // "Consume" the data.
			
			if(data.endOfNextProducer != null) // Create next consumer, which runs after the next producer.
				Intervals.intervalWithBound(currentEnd.bound())
					.startAfter(currentEnd)
					.startAfter(data.endOfNextProducer)
					.schedule(new Consumer(data.dataForNextProducer));
		}
	}
	
	@Test public void test() {
		blockingInterval(new Task() {
			public void run(Point currentEnd) {
				ProducerData data0 = new ProducerData();
				Interval prod0 = Intervals.intervalWithBound(currentEnd)
					.schedule(new Producer(data0, 0));
				Intervals.intervalWithBound(currentEnd)
					.startAfter(prod0.end())
					.schedule(new Consumer(data0));
			}			
		});
		for(int i = 0; i < MAX; i++)
			assertEquals(i, consumed.get(i).intValue());
	}
	
}
