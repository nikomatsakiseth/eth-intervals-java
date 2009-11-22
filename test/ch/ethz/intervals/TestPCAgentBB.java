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
 * Similar to {@link TestPCAgent}, except done in 
 * bounded-buffer style.
 */
public class TestPCAgentBB {
		
	final int MAX = 100;
	List<Integer> consumed = new ArrayList<Integer>();
	
	final AtomicInteger stamp = new AtomicInteger();
	List<Integer> producerTimes = new ArrayList<Integer>();
	List<Integer> consumerTimes = new ArrayList<Integer>();
	
	final int BBSIZE = 3;
	
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
	
	class ConsumerResult {
		public final EndPoint<ConsumerResult> nextConsumerInterval;

		public ConsumerResult(EndPoint<ConsumerResult> nextConsumerInterval) {
			this.nextConsumerInterval = nextConsumerInterval;
		}
	}
	
	class Producer implements Task<ProducerResult> {
		private final int index;
		private EndPoint<ConsumerResult> waitForConsumer;

		public Producer(int index, EndPoint<ConsumerResult> waitForConsumer) {
			this.index = index;
			this.waitForConsumer = waitForConsumer;
		}
		
		@Override
		public String toString() {
			return "Producer["+index+"]";
		}

		public ProducerResult run(Interval<ProducerResult> current) {
			Interval<ProducerResult> nextProducerInterval;
			
			producerTimes.add(stamp.getAndIncrement());
			
			EndPoint<ConsumerResult> nextConsumerToWaitFor;
			if(index < BBSIZE) {
				nextConsumerToWaitFor = waitForConsumer;
			} else {
				nextConsumerToWaitFor = waitForConsumer.result().nextConsumerInterval;
			}
			
			final int nextIndex = index + 1;
			if(nextIndex < MAX) // Create next producer, if any.
				nextProducerInterval = 
					intervalWithBound(current.bound())
					.startAfter(current.end())
					.startAfter((nextIndex < BBSIZE ? null : nextConsumerToWaitFor))
					.schedule(new Producer(nextIndex, nextConsumerToWaitFor));
			else
				nextProducerInterval = null;
			
			return new ProducerResult(index, Intervals.end(nextProducerInterval));
		}
	}
	
	class Consumer implements Task<ConsumerResult> {
		private final int index;
		private final EndPoint<ProducerResult> producerInterval;
		
		public Consumer(int index, EndPoint<ProducerResult> producerInterval) {
			this.index = index;
			this.producerInterval = producerInterval;
		}

		@Override
		public String toString() {
			return "Consumer["+index+"]";
		}

		@Override
		public ConsumerResult run(Interval<ConsumerResult> current) {
			ProducerResult result = producerInterval.result(); // Safe as producer.end -> consumer.start
			
			consumerTimes.add(stamp.getAndIncrement());			
			consumed.add(result.produced); // "Consume" the data.
			
			if(result.nextProducerInterval != null) { // Create next consumer, which runs after the next producer.
				Interval<ConsumerResult> nextConsumerInterval = 
					intervalWithBound(current.bound())
					.startAfter(current.end())
					.startAfter(result.nextProducerInterval)
					.schedule(new Consumer(index + 1, result.nextProducerInterval));
				return new ConsumerResult(nextConsumerInterval.end());
			}
			return null;
		}
	}
	
	@Test public void test() {
		blockingInterval(new Task<Void>() {
			public Void run(final Interval<Void> parent) {
				
				intervalWithBound(parent.end()).schedule(new Task<Void>() {
					public Void run(final Interval<Void> setup) {
						Producer firstProducer = new Producer(0, null); 
				
						Interval<ProducerResult> firstProducerInterval = 
							intervalWithBound(parent.end())
							.startAfter(setup.end())
							.schedule(firstProducer);
				
						Interval<ConsumerResult> firstConsumerInterval = 
							intervalWithBound(parent.end())
							.startAfter(setup.end())
							.startAfter(end(firstProducerInterval))
							.schedule(new Consumer(0, firstProducerInterval.end()));
						
						firstProducer.waitForConsumer = firstConsumerInterval.end();
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
						"prod " + i + " ran at " + pTime + " but consumer ran at " + cTime, 
						pTime > cTime);
			}
		}
	}
	
}
