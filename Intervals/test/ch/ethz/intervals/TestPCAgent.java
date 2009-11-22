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
	
	class ProducerData {
		public final Integer produced;
		public final IntervalFuture<ProducerData> nextProducerFuture;
		
		public ProducerData(
				Integer produced,
				IntervalFuture<ProducerData> nextProducerFuture) 
		{
			this.produced = produced;
			this.nextProducerFuture = nextProducerFuture;
		}
	}
	
	class Producer implements Task<ProducerData> {
		private final int index;
		
		private Producer(int index) {
			this.index = index;
		}

		public ProducerData run(Interval<ProducerData> current) {
			IntervalFuture<ProducerData> nextProducerFuture = null;
			if(index + 1 < MAX) // Create next producer, if any.
				nextProducerFuture = Intervals.intervalWithBound(current.bound())
				.startAfter(end(current))
				.schedule(new Producer(index + 1)).future();
			else
				nextProducerFuture = null;
			
			return new ProducerData(index, nextProducerFuture);
		}
	}
	
	class Consumer implements Task<Void> {
		private final IntervalFuture<ProducerData> producerInterval;
		
		public Consumer(IntervalFuture<ProducerData> producerInterval) {
			this.producerInterval = producerInterval;
		}

		public Void run(Interval<Void> current) {
			ProducerData result = producerInterval.result(); // Safe as producer.end -> consumer.start
			
			consumed.add(result.produced); // "Consume" the data.
			
			if(result.nextProducerFuture != null) // Create next consumer, which runs after the next producer.
				Intervals.intervalWithBound(current.bound())
				.startAfter(result.nextProducerFuture.end())
				.schedule(new Consumer(result.nextProducerFuture));
			return null;
		}
	}
	
	@Test public void test() {
		blockingInterval(new Task<Void>() {
			public Void run(Interval<Void> current) {
				Interval<ProducerData> firstProducerInterval = 
					intervalDuring(current).schedule(new Producer(0));
				intervalDuring(current)
					.startAfter(end(firstProducerInterval))
					.schedule(new Consumer(firstProducerInterval.future()));
				return null;
			}			
		});
		for(int i = 0; i < MAX; i++)
			assertEquals(i, consumed.get(i).intValue());
	}
	
}
