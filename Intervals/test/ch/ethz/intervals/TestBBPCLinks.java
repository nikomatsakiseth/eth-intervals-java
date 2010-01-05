package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.subinterval;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Assert;
import org.junit.Test;

import ch.ethz.intervals.quals.WrittenDuring;
import ch.ethz.intervals.quals.Creator;

/**
 * Bounded-buffer Producer Consumer example
 * using linked lists.  Includes annotations
 * for our static type system that provide it
 * is free of data races..
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
	@Creator class ProducerData {
		/** Data producer by producer <em>i</em> */
		@WrittenDuring("writer")
		public int produced;
		
		/** Producer <em>i+1</em> */
		@WrittenDuring("writer")
		public Interval nextProducer;
		
		/** Where producer <em>i+1</em> will write its data */
		@WrittenDuring("writer")
		public @Creator("nextProducer") ProducerData nextProducerData;		
	}
	
	/** Where consumers write information about the next consumer. */
	@Creator class ConsumerData {
		/** Consumer <em>i+1</em> */
		@WrittenDuring("writer")
		public Interval nextConsumer;
		
		/** Where consumer <em>i+1</em> will write its data */
		@WrittenDuring("writer")
		public @Creator("nextConsumer") ConsumerData nextConsumerData;		
	}
	
	class Producer extends Interval {
		@WrittenDuring("constructor")
		protected final int index;
		@WrittenDuring("constructor")
		protected final @Creator("hb this") ConsumerData cdata;
		@WrittenDuring("constructor")
		protected final @Creator("this") ProducerData pdata;
		
		public Producer(
				int index,
				Interval prev,
				Interval cons,
				@Creator("cons") ConsumerData cdata) 
		{
			super(prev.end.bound);
			Intervals.addHb(prev.end, start);
			Intervals.addHb(cons.end, start);
			
			this.index = index;
			this.cdata = cdata;
			this.pdata = new /*@writer("this")*/ ProducerData();
		}

		public void run() {
			pdata.produced = index;
			producerTimes.add(stamp.getAndIncrement());
			
			if(index + 1 < MAX) { 
				// Create next producer, if any.  Publish its data and endpoint.
				Producer nextProducer = new Producer(index + 1, this, cdata.nextConsumer, cdata.nextConsumerData);
				pdata.nextProducerData = nextProducer.pdata;
				pdata.nextProducer = nextProducer;
			} 
		}		
	}
	
	class Consumer extends Interval {
		@WrittenDuring("constructor")
		protected final int index;
		@WrittenDuring("constructor")
		protected final @Creator("hb this") ProducerData pdata;
		@WrittenDuring("constructor")
		protected final @Creator("this") ConsumerData cdata;

		public Consumer(
				int index,
				Interval prevConsumer,
				Interval producer,
				@Creator("producer") ProducerData pdata) 
		{
			super(prevConsumer.end.bound);
			Intervals.addHb(prevConsumer.end, start);
			Intervals.addHb(producer.end, start);
			
			this.index = index;
			this.pdata = pdata;
			this.cdata = new /*@writer("this")*/ ConsumerData();
		}
		
		public String toString() {
			return "Consumer["+index+"]";
		}
		
		@Override public void run() {
			consumed.add(pdata.produced); // "Consume" the data.
			consumerTimes.add(stamp.getAndIncrement());			
			
			if(pdata.nextProducer != null) {
				Consumer nextConsumer = new Consumer(
						index + 1,
						this,
						pdata.nextProducer,
						pdata.nextProducerData);
				cdata.nextConsumer = nextConsumer;
				cdata.nextConsumerData = nextConsumer.cdata;
			}
		}		
	}
	
	class Init extends Interval {

		public Init(Dependency dep) {
			super(dep);
		}

		@Override
		protected void run() {
			// Create a dummy chain of consumer datas from index -N to -1.
			// These consumer datas are all written by this interval.
			@Creator("this") ConsumerData cdata_1 = new /*@writer("this")*/ ConsumerData();			
			@Creator("this") ConsumerData cdata_N = cdata_1;
			for(int i = -1; i <= -N; i--) {
				@Creator("this") ConsumerData cdata_i = new /*@writer("this")*/ ConsumerData();
				cdata_i.nextConsumer = this;
				cdata_i.nextConsumerData = cdata_N;
				cdata_N = cdata_i;
			}
			
			// Create first producer, and provide it cdata -N, so that
			// prod N will wait for cons 0:
			Producer prod0 = new Producer(0, this, this, cdata_N);
			
			// Create first consumer, and link its cdata to cdata -1:
			Consumer cons0 = new Consumer(0, this, prod0, prod0.pdata);
			cdata_1.nextConsumer = cons0;
			cdata_1.nextConsumerData = cons0.cdata;
		}
	
	}
	
	@Test public final void test() {
		subinterval(new VoidSubinterval() {
			@Override public void run(Interval subinterval) {
				new Init(subinterval);
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
