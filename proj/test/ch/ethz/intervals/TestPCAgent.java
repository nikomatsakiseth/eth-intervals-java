package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.end;
import static ch.ethz.intervals.Intervals.blockingInterval;
import static ch.ethz.intervals.Intervals.intervalDuring;
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
 * {@link Producer} simply returns a {@link ProducerResult} instance
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
	
	class ProducerResult {
		public final Integer produced;
		public final EndPoint<ProducerResult> nextProducerInterval;
		
		public ProducerResult(
				Integer produced,
				EndPoint<ProducerResult> nextProducerInterval) 
		{
			this.produced = produced;
			this.nextProducerInterval = nextProducerInterval;
		}
	}
	
	class Producer implements Task<ProducerResult> {
		private final int index;
		
		private Producer(int index) {
			this.index = index;
		}

		public ProducerResult run(Interval<ProducerResult> current) {
			Interval<ProducerResult> nextProducerInterval;
			
			if(index + 1 < MAX) // Create next producer, if any.
				nextProducerInterval = Intervals.intervalWithBound(current.bound())
				.startAfter(end(current))
				.schedule(new Producer(index + 1));
			else
				nextProducerInterval = null;
			
			return new ProducerResult(index, Intervals.end(nextProducerInterval));
		}
	}
	
	class Consumer implements Task<Void> {
		private final EndPoint<ProducerResult> producerInterval;
		
		public Consumer(EndPoint<ProducerResult> producerInterval) {
			this.producerInterval = producerInterval;
		}

		public Void run(Interval<Void> current) {
			ProducerResult result = producerInterval.result(); // Safe as producer.end -> consumer.start
			
			consumed.add(result.produced); // "Consume" the data.
			
			if(result.nextProducerInterval != null) // Create next consumer, which runs after the next producer.
				Intervals.intervalWithBound(current.bound())
				.startAfter(result.nextProducerInterval)
				.schedule(new Consumer(result.nextProducerInterval));
			return null;
		}
	}
	
	@Test public void test() {
		blockingInterval(new Task<Void>() {
			public Void run(Interval<Void> current) {
				Interval<ProducerResult> firstProducerInterval = 
					intervalDuring(current).schedule(new Producer(0));
				intervalDuring(current)
					.startAfter(end(firstProducerInterval))
					.schedule(new Consumer(firstProducerInterval.end()));
				return null;
			}			
		});
		for(int i = 0; i < MAX; i++)
			assertEquals(i, consumed.get(i).intValue());
	}
	
}
