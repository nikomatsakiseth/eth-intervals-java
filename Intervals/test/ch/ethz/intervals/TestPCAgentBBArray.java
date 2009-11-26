package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.blockingInterval;
import static ch.ethz.intervals.Intervals.intervalWithBound;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Assert;
import org.junit.Test;

import ch.ethz.intervals.TestPCAgent.Consumer;
import ch.ethz.intervals.TestPCAgent.Producer;
import ch.ethz.intervals.TestPCAgent.ProducerData;

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
	
	Point consumerEndPoints[] = new Point[BBSIZE];
	
	final AtomicInteger stamp = new AtomicInteger();
	List<Integer> producerTimes = new ArrayList<Integer>();
	List<Integer> consumerTimes = new ArrayList<Integer>();
	
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

		public String toString() {
			return "Producer["+index+"]";
		}

		public void run(Point currentEnd) {
			data.produced = index;
			producerTimes.add(stamp.getAndIncrement());
			
			final int nextIndex = index + 1;
			Point waitForConsumerPoint;
			if(nextIndex >= BBSIZE)
				waitForConsumerPoint = consumerEndPoints[(nextIndex - BBSIZE) % BBSIZE];
			else
				waitForConsumerPoint = null;
			
			if(index + 1 < MAX) { // Create next producer, if any.
				data.dataForNextProducer = new ProducerData();
				Producer nextProducer = new Producer(data.dataForNextProducer, nextIndex);
				data.endOfNextProducer = 
					Intervals.intervalWithBound(currentEnd.bound())
						.startAfter(currentEnd)
					.startAfter(waitForConsumerPoint)
						.schedule(nextProducer)
						.end();
			} 
		}
	}
	
	class Consumer extends AbstractTask {
		private final int index;
		private final ProducerData data;

		public Consumer(int index, ProducerData data) {
			this.index = index;
			this.data = data;
		}
		
		public String toString() {
			return "Consumer["+index+"]";
		}

		public void run(Point currentEnd) {
			consumed.add(data.produced); // "Consume" the data.
			consumerTimes.add(stamp.getAndIncrement());
			
			if(data.endOfNextProducer != null) { // Create next consumer, which runs after the next producer.
				final int nextIndex = index + 1;
				Interval nextConsumer = Intervals.intervalWithBound(currentEnd.bound())
					.startAfter(currentEnd)
					.startAfter(data.endOfNextProducer)
					.schedule(new Consumer(nextIndex, data.dataForNextProducer));
				consumerEndPoints[nextIndex % BBSIZE] = nextConsumer.end();
			}
		}
	}
	
	@Test public void test() {
		
		blockingInterval(new SetupTask() {
			
			@Override
			public void setup(Point setupEnd, Interval worker) {
				ProducerData pdata = new ProducerData(); 
				
				Interval prod0 = Intervals.intervalDuring(worker) 
					.schedule(new Producer(pdata, 0));
				
				Interval cons0 = Intervals.intervalDuring(worker)
					.startAfter(prod0.end())
					.schedule(new Consumer(0, pdata));

				consumerEndPoints[0] = cons0.end();	
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
