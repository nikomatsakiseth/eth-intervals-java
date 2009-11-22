package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.end;
import static ch.ethz.intervals.Intervals.blockingInterval;
import static ch.ethz.intervals.Intervals.intervalDuring;
import static ch.ethz.intervals.Intervals.intervalWithBound;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Assert;
import org.junit.Test;

/**
 * Bounded-Buffer Agent Producer-Consumer Example
 *
 * Similar to {@link TestPCAgentBB}, except done in 
 * bounded-buffer style, using an array.
 */
public class TestPCAgentBBArray {
	
	final int MAX = 100;
	List<Integer> consumed = new ArrayList<Integer>();
	
	final int BBSIZE = 3;
	
	@SuppressWarnings("unchecked")
	EndPoint<Void> consumerEndPoints[] = new EndPoint[BBSIZE];
	
	final AtomicInteger stamp = new AtomicInteger();
	List<Integer> producerTimes = new ArrayList<Integer>();
	List<Integer> consumerTimes = new ArrayList<Integer>();
	
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
			
			producerTimes.add(stamp.getAndIncrement());
			
			if(index + 1 < MAX) { // Create next producer, if any.
				final int nextIndex = index + 1;
				
				EndPoint<?> waitForConsumerPoint;
				if(nextIndex >= BBSIZE)
					waitForConsumerPoint = consumerEndPoints[(nextIndex - BBSIZE) % BBSIZE];
				else
					waitForConsumerPoint = null;
				
				nextProducerInterval = 
					intervalWithBound(current.bound())
					.startAfter(current.end())
					.startAfter(waitForConsumerPoint)
					.schedule(new Producer(nextIndex));
			} else
				nextProducerInterval = null;
			
			return new ProducerResult(index, Intervals.end(nextProducerInterval));
		}
	}
	
	class Consumer implements Task<Void> {
		private final int index;
		private final EndPoint<ProducerResult> producerInterval;
		
		public Consumer(int index, EndPoint<ProducerResult> producerInterval) {
			this.index = index;
			this.producerInterval = producerInterval;
		}

		public Void run(Interval<Void> current) {
			ProducerResult result = producerInterval.result(); // Safe as producer.end -> consumer.start
			
			consumerTimes.add(stamp.getAndIncrement());			
			consumed.add(result.produced); // "Consume" the data.
			
			if(result.nextProducerInterval != null) { // Create next consumer, which runs after the next producer.
				final int nextIndex = index + 1;
				Interval<Void> nextConsumer = 
					intervalWithBound(current.bound())
					.startAfter(current.end())
					.startAfter(result.nextProducerInterval)
					.schedule(new Consumer(nextIndex, result.nextProducerInterval));
				consumerEndPoints[nextIndex % BBSIZE] = nextConsumer.end();
			}
			return null;
		}
	}
	
	@Test public void test() {
		
		blockingInterval(new Task<Void>() {
			public Void run(final Interval<Void> parent) {
				
				intervalWithBound(parent.end()).schedule(new Task<Void>() {
					public Void run(final Interval<Void> setup) {
						Interval<ProducerResult> firstProducerInterval = 
							intervalWithBound(parent.end())
							.startAfter(setup.end())
							.schedule(new Producer(0));
						
						Interval<Void> firstConsumerInterval = 
							intervalWithBound(parent.end())
							.startAfter(setup.end())
							.startAfter(firstProducerInterval.end())
							.schedule(new Consumer(0, firstProducerInterval.end()));

						consumerEndPoints[0] = firstConsumerInterval.end();	
						return null;
					}					
				});
				
				return null;
			}			
		});
		
		for(int i = 0; i < MAX; i++) {
			assertEquals(i, consumed.get(i).intValue());
			
			if(i >= BBSIZE) {
				int pTime = producerTimes.get(i);
				int cTime = consumerTimes.get(i - BBSIZE);
				
				Assert.assertTrue(
						"prod " + i + " ran at " + pTime + " but consumer " + (i - BBSIZE) + " ran at " + cTime, 
						pTime > cTime);
			}
		}
	}
	
}
