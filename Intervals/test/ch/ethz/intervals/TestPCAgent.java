package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.blockingInterval;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Assert;
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
	
	final int MAX = 100;
	List<Integer> consumed = new ArrayList<Integer>();
	
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
		protected final ProducerData pdata;
		protected final int index;
		
		public Producer(int index, ProducerData producerData) {
			this.pdata = producerData;
			this.index = index;
		}

		public void run(Point currentEnd) {
			pdata.produced = index;
			producerTimes.add(stamp.getAndIncrement());
			
			if(index + 1 < MAX) { 
				// Create next producer, if any.  Publish its data and endpoint.
				pdata.dataForNextProducer = new ProducerData();
				pdata.endOfNextProducer = nextProducer(pdata.dataForNextProducer).end(); 
			} 
		}
		
		protected Interval nextProducer(ProducerData dataForNextProducer) {
			return Intervals.successorInterval(
					new Producer(index + 1, dataForNextProducer));			
		}
	}
	
	class Consumer extends AbstractTask {
		protected final int index;
		protected final Point endOfProducer;
		protected final ProducerData pdata;

		public Consumer(int index, Point endOfProducer, ProducerData pdata) {
			this.index = index;
			this.endOfProducer = endOfProducer;
			this.pdata = pdata;
		}
		
		public String toString() {
			return "Consumer["+index+"]";
		}
		
		@Override
		public void addDependencies(Interval inter) {
			super.addDependencies(inter);
			Intervals.addHb(endOfProducer, inter.start());
		}

		public void run(Point _) {
			consumed.add(pdata.produced); // "Consume" the data.
			consumerTimes.add(stamp.getAndIncrement());			
			
			if(pdata.endOfNextProducer != null) {
				// Create next consumer, which runs after the next producer.
				nextConsumer();
			}
		}
		
		protected void nextConsumer() {
			Intervals.successorInterval(
					new Consumer(index + 1, pdata.endOfNextProducer, pdata.dataForNextProducer));
		}
	}
	
	@Test public final void test() {
		runProducerConsumer();		
		checkResults();
	}

	protected void runProducerConsumer() {
		blockingInterval(new AbstractTask() {
			public void run(Point _) {
				ProducerData data0 = new ProducerData();
				Interval prod0 = Intervals.childInterval(new Producer(0, data0));
				Intervals.childInterval(new Consumer(0, prod0.end(), data0));
			}			
		});
	}
	
	protected void checkResults() {
		for(int i = 0; i < MAX; i++) {
			assertEquals(i, consumed.get(i).intValue());
		}
		for(int i = 1; i < MAX; i++) {
			Assert.assertTrue("Producer out of order", producerTimes.get(i) > producerTimes.get(i-1));
			Assert.assertTrue("Consumer out of order", consumerTimes.get(i) > consumerTimes.get(i-1));
		}
	}
	
}
