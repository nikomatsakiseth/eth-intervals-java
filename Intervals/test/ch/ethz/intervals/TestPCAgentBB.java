package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.blockingInterval;

import org.junit.Assert;

/**
 * Bounded-Buffer Agent Producer-Consumer Example
 *
 * Similar to {@link TestPCAgent}, except done in 
 * bounded-buffer style.
 */
public class TestPCAgentBB extends TestPCAgent {
		
	final int BBSIZE = 3;
	
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
	
	class ProducerBB extends Producer {
		
		private final ConsumerData cdataMN; /** Data for consumer #(index-BBSIZE) */

		public ProducerBB(int index, ProducerData producerData, ConsumerData cdataMN) {
			super(index, producerData);
			assert cdataMN != null;
			this.cdataMN = cdataMN;
		}

		@Override
		protected Interval nextProducer(ProducerData dataForNextProducer) {	
			assert cdataMN.dataForNextConsumer != null;
			Interval inter = Intervals.successorInterval(new ProducerBB(
					index + 1, 
					dataForNextProducer, 
					cdataMN.dataForNextConsumer));
			Intervals.addHb(cdataMN.endOfNextConsumer, inter.start());
			return inter;
		}
		
	}
	
	class ConsumerBB extends Consumer {

		private final ConsumerData cdata; /** Data for consumer #(index-BBSIZE) */
		
		public ConsumerBB(int index, Point endOfProducer, ProducerData pdata, ConsumerData cdata) {
			super(index, endOfProducer, pdata);
			this.cdata = cdata;
		}
		
		@Override
		protected void nextConsumer() {
			cdata.dataForNextConsumer = new ConsumerData(null, null);
			Interval inter = Intervals.successorInterval(new ConsumerBB(
					index + 1,
					pdata.endOfNextProducer,
					pdata.dataForNextProducer,
					cdata.dataForNextConsumer));
			cdata.endOfNextConsumer = inter.end();
		}

		
	}
	
	@Override 
	protected void runProducerConsumer() {
		blockingInterval(new AbstractTask() {
			
			@Override
			public void run(Point _) {
				// Data for the 0th producer and consumer:
				ProducerData pdata0 = new ProducerData();				
				ConsumerData cdata0 = new ConsumerData(null, null);
				
				// Create "BBSIZE" dummy entries to give the producer
				// some room to execute before it starts to wait on the consumers:
				ConsumerData cdataM1 = new ConsumerData(null, cdata0); // cdata -1
				ConsumerData cdataMN = cdataM1;                        // cdata -N
				for(int i = -2; i >= -BBSIZE; i--)
					cdataMN = new ConsumerData(null, cdataMN); // create cdata -i
				
				Interval p0 = Intervals.childInterval(new ProducerBB(0, pdata0, cdataMN));
				Interval c0 = Intervals.childInterval(new ConsumerBB(0, p0.end(), pdata0, cdata0));
				cdataM1.endOfNextConsumer = c0.end();
			}
		});
	}
	
	@Override
	protected void checkResults() {
		super.checkResults();
		
		for(int i = 0; i < MAX; i++) {
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
