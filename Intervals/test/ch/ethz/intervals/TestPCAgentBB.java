package ch.ethz.intervals;

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
	
	/** Where the consumer writes its data.  There is one 
	 *  instance per consumer. It is not safe to read these
	 *  fields unless the end of the consumer <em>happens before</em>
	 *  the interval which is reading. */
	class ConsumerData {
		public Point endOfNextConsumer;
		public ConsumerData dataForNextConsumer;
		
		public ConsumerData(
				Point endOfNextConsumer,
				ConsumerData dataForNextConsumer) 
		{
			this.endOfNextConsumer = endOfNextConsumer;
			this.dataForNextConsumer = dataForNextConsumer;
		}
	}
	
	class Producer extends AbstractTask {
		private final ProducerData pdata;
		private final ConsumerData cdata;
		private final int index;
		
		public Producer(int index, ProducerData pdata, ConsumerData cdata) {
			this.index = index;
			this.pdata = pdata;
			this.cdata = cdata;
		}
		
		@Override
		public String toString() {
			return "Producer["+index+"]";
		}
		
		public void run(Point currentEnd) {
			pdata.produced = index;
			producerTimes.add(stamp.getAndIncrement());
			
			final int nextIndex = index + 1;
			if(nextIndex < MAX) { // Create next producer, if any.
				pdata.dataForNextProducer = new ProducerData();
				Producer nextProducer = new Producer(
						nextIndex, pdata.dataForNextProducer, cdata.dataForNextConsumer);
				pdata.endOfNextProducer =
					intervalWithBound(currentEnd.bound())
						.startAfter(currentEnd)
						.startAfter(cdata.endOfNextConsumer)
						.schedule(nextProducer)
						.end();
			}
		}
	}
	
	class Consumer extends AbstractTask {
		private final int index;
		private final ProducerData pdata;
		private final ConsumerData cdata;

		public Consumer(int index, ProducerData pdata, ConsumerData cdata) {
			this.index = index;
			this.pdata = pdata;
			this.cdata = cdata;
		}

		@Override
		public String toString() {
			return "Consumer["+index+"]";
		}
		
		public void run(Point currentEnd) {
			consumed.add(pdata.produced); // "Consume" the data.
			consumerTimes.add(stamp.getAndIncrement());			
			
			if(pdata.endOfNextProducer != null) { // Create next consumer, which runs after the next producer.
				cdata.dataForNextConsumer = new ConsumerData(null, null);
				cdata.endOfNextConsumer = 
					Intervals.intervalWithBound(currentEnd.bound())
						.startAfter(currentEnd)
						.startAfter(pdata.endOfNextProducer)
						.schedule(new Consumer(
								index + 1,
								pdata.dataForNextProducer, 
								cdata.dataForNextConsumer))
						.end();
			}
		}
	}
	
	@Test public void test() {
		blockingInterval(new SetupTask() {			
			@Override
			public void setup(Point setupEnd, Interval worker) {
				// Data for the 0th producer and consumer:
				ProducerData pdata0 = new ProducerData();				
				ConsumerData cdata0 = new ConsumerData(null, null);
				
				// Create "BBSIZE" dummy entries to give the producer
				// some room to execute before it starts to wait on the consumers:
				ConsumerData cdataN = cdata0; // "cdata # negative"
				for(int i = 0; i < BBSIZE; i++)
					cdataN = new ConsumerData(null, cdataN);
				
				Interval firstProducerInterval = intervalDuring(worker)
					.schedule(new Producer(0, pdata0, cdataN));
		
				intervalDuring(worker)
					.startAfter(firstProducerInterval.end())
					.schedule(new Consumer(0, pdata0, cdata0));
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
