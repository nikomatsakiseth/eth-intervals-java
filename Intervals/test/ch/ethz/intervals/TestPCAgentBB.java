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
	
	class ConsumerData {
		public final IntervalFuture<ConsumerData> nextConsumerFuture;

		public ConsumerData(IntervalFuture<ConsumerData> nextConsumerInterval) {
			this.nextConsumerFuture = nextConsumerInterval;
		}
	}
	
	class Producer implements Task<ProducerData> {
		private final int index;
		private IntervalFuture<ConsumerData> waitForConsumer;

		public Producer(int index, IntervalFuture<ConsumerData> waitForConsumer) {
			this.index = index;
			this.waitForConsumer = waitForConsumer;
		}
		
		@Override
		public String toString() {
			return "Producer["+index+"]";
		}

		public ProducerData run(Interval<ProducerData> current) {
			IntervalFuture<ProducerData> nextProducerFuture;
			
			producerTimes.add(stamp.getAndIncrement());
			
			IntervalFuture<ConsumerData> nextConsumerToWaitFor;
			if(index < BBSIZE) {
				nextConsumerToWaitFor = waitForConsumer;
			} else {
				nextConsumerToWaitFor = waitForConsumer.result().nextConsumerFuture;
			}
			
			final int nextIndex = index + 1;
			if(nextIndex < MAX) // Create next producer, if any.
				nextProducerFuture = 
					intervalWithBound(current.bound())
					.startAfter(current.end())
					.startAfter((nextIndex < BBSIZE ? null : nextConsumerToWaitFor.end()))
					.schedule(new Producer(nextIndex, nextConsumerToWaitFor))
					.future();
			else
				nextProducerFuture = null;
			
			return new ProducerData(index, nextProducerFuture);
		}
	}
	
	class Consumer implements Task<ConsumerData> {
		private final int index;
		private final IntervalFuture<ProducerData> producerInterval;
		
		public Consumer(int index, IntervalFuture<ProducerData> producerInterval) {
			this.index = index;
			this.producerInterval = producerInterval;
		}

		@Override
		public String toString() {
			return "Consumer["+index+"]";
		}

		@Override
		public ConsumerData run(Interval<ConsumerData> current) {
			ProducerData result = producerInterval.result(); // Safe as producer.end -> consumer.start
			
			consumerTimes.add(stamp.getAndIncrement());			
			consumed.add(result.produced); // "Consume" the data.
			
			if(result.nextProducerFuture != null) { // Create next consumer, which runs after the next producer.
				Interval<ConsumerData> nextConsumerInterval = 
					intervalWithBound(current.bound())
					.startAfter(current.end())
					.startAfter(result.nextProducerFuture.end())
					.schedule(new Consumer(index + 1, result.nextProducerFuture));
				return new ConsumerData(nextConsumerInterval.future());
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
				
						Interval<ProducerData> firstProducerInterval = 
							intervalWithBound(parent.end())
							.startAfter(setup.end())
							.schedule(firstProducer);
				
						Interval<ConsumerData> firstConsumerInterval = 
							intervalWithBound(parent.end())
							.startAfter(setup.end())
							.startAfter(end(firstProducerInterval))
							.schedule(new Consumer(0, firstProducerInterval.future()));
						
						firstProducer.waitForConsumer = firstConsumerInterval.future();
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
