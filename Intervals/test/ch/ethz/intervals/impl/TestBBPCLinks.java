package ch.ethz.intervals.impl;

import static ch.ethz.intervals.Intervals.inline;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Assert;
import org.junit.Test;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.quals.Creator;
import ch.ethz.intervals.task.AbstractTask;

/**
 * Bounded-buffer Producer Consumer example
 * using linked lists.  
 */
public class TestBBPCLinks {

	/**
	 * Maximum number of outstanding producers (alternatively, size of bounded buffer).
	 * Producer N waits to Consumer 0.
	 */
	static final int N = 2;
	
	/** Number of items to produce. */
	static final int MAX = 100;
	
	/** Data item consumed by each producer. */
	List<Integer> consumed = new ArrayList<Integer>();
	
	/** Used to obtain atomic timestamps. */
	final AtomicInteger stamp = new AtomicInteger();
	
	/** Time stamp that each producer executed. */
	List<Integer> producerTimes = new ArrayList<Integer>();
	
	/** Time stamp that each consumer executed. */
	List<Integer> consumerTimes = new ArrayList<Integer>();	
	
	/** Where producers write the data they produce,
	 *  and information about the next producer. */
	class ProducerData {
		/** Data producer by producer <em>i</em> */
		public int produced;
		
		/** Producer <em>i+1</em> */
		public Interval nextProducer;
		
		/** Where producer <em>i+1</em> will write its data */
		public ProducerData nextProducerData;		
	}
	
	/** Where consumers write information about the next consumer. */
	class ConsumerData {
		/** Consumer <em>i+1</em> */
		public Interval nextConsumer;
		
		/** Where consumer <em>i+1</em> will write its data */
		public ConsumerData nextConsumerData;		
	}
	
	public void newProducer(
			int index,
			Interval prevProducer,
			ProducerData prevPdata,
			Interval consumer,
			ConsumerData cdata
	) {
		Producer nextProducer = new Producer(index, cdata);
		Interval inter = prevProducer.getParent().newAsyncChild(nextProducer);
		
		prevPdata.nextProducerData = nextProducer.pdata;
		prevPdata.nextProducer = inter;
			
		Intervals.addHb(prevProducer, inter);
		Intervals.addHb(consumer, inter);
	}
	
	class Producer extends AbstractTask {
		protected final int index;
		protected final ConsumerData cdata;
		protected final ProducerData pdata;
		
		public Producer(int index, ConsumerData cdata)
		{
			super("p"+index);
			this.index = index;
			this.cdata = cdata;
			this.pdata = new /*@writer("this")*/ ProducerData();
		}

		public void run(Interval current) {
			pdata.produced = index;
			producerTimes.add(stamp.getAndIncrement());
			
			if(index + 1 < MAX) { 
				// Create next producer, if any.  Publish its data and endpoint.
				newProducer(index + 1, current, pdata, cdata.nextConsumer, cdata.nextConsumerData);
			} 
		}		
	}
	
	public void newConsumer(
			int index,
			Interval producer,
			ProducerData pdata,
			Interval prevConsumer,
			ConsumerData prevCdata
	) {
		Consumer nextConsumer = new Consumer(index, pdata);
		Interval inter = prevConsumer.getParent().newAsyncChild(nextConsumer);
		
		Intervals.addHb(prevConsumer, inter);
		Intervals.addHb(producer, inter);
		
		prevCdata.nextConsumer = inter;
		prevCdata.nextConsumerData = nextConsumer.cdata;
	}
	
	class Consumer extends AbstractTask {
		protected final int index;
		protected final @Creator("hb this") ProducerData pdata;
		protected final @Creator("this") ConsumerData cdata;

		public Consumer(
				int index,
				@Creator("producer") ProducerData pdata) 
		{
			super("c"+index);
			
			this.index = index;
			this.pdata = pdata;
			this.cdata = new /*@writer("this")*/ ConsumerData();
		}
		
		@Override public void run(Interval current) {
			consumed.add(pdata.produced); // "Consume" the data.
			consumerTimes.add(stamp.getAndIncrement());			
			
			if(pdata.nextProducer != null) {
				newConsumer(index + 1, pdata.nextProducer, pdata.nextProducerData, current, cdata);
			}
		}		
	}
	
	class Init extends AbstractTask {

		public Init() {
			super("Init");
		}

		@Override
		public void run(Interval current) {
			// Create a dummy chain of consumer datas from index -N to -1.
			// These consumer datas are all written by this interval.
			ConsumerData cdata_1 = new ConsumerData();			
			ConsumerData cdata_N = cdata_1;
			for(int i = -1; i <= -N; i--) {
				ConsumerData cdata_i = new ConsumerData();
				cdata_i.nextConsumer = current;
				cdata_i.nextConsumerData = cdata_N;
				cdata_N = cdata_i;
			}
			
			// Create first producer, and provide it cdata -N, so that
			// prod N will wait for cons 0:
			ProducerData pdata_1 = new ProducerData();
			newProducer(0, current, pdata_1, current, cdata_N);
			
			// Create first consumer, and link its cdata to cdata -1:
			newConsumer(0, pdata_1.nextProducer, pdata_1.nextProducerData, current, cdata_1);
		}
	
	}
	
	@Test public final void test() {
		inline(new AbstractTask() {
			@Override public void run(Interval subinterval) {
				subinterval.newAsyncChild(new Init());
			}			
		});
		
		checkResults();
	}

	protected void checkResults() {
		for(int i = 0; i < MAX; i++) {
			assertEquals(i, consumed.get(i).intValue());
		}
		for(int i = 1; i < MAX; i++) {
			Assert.assertTrue("Producer out of order", producerTimes.get(i) > producerTimes.get(i-1));
			Assert.assertTrue("Consumer out of order", consumerTimes.get(i) > consumerTimes.get(i-1));
		}		
		for(int i = 0; i < MAX; i++) {
			if(i >= N) {
				int pTime = producerTimes.get(i);
				int cTime = consumerTimes.get(i - N);
				
				Assert.assertTrue(
						"prod " + i + " ran at " + pTime + " but consumer ran at " + cTime, 
						pTime > cTime);
			}
		}
	}
	
}
