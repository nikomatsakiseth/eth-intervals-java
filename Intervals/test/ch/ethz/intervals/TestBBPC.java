package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.blockingInterval;
import static ch.ethz.intervals.Intervals.end;
import static ch.ethz.intervals.Intervals.intervalDuring;
import static ch.ethz.intervals.Intervals.intervalWithBound;
import static ch.ethz.intervals.Intervals.start;

import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Assert;
import org.junit.Test;

import ch.ethz.intervals.quals.ObjectParameter;

@ObjectParameter
@interface BBPC_P {
    String value() default "BBPC_P";
}

@ObjectParameter
@interface BBPC_C {
    String value() default "BBPC_C";
}

/**
 * Bounded-Buffer Producer Consumer Example.
 *
 */
public class TestBBPC {
	
	public final int N = 3;
	public final int M = 100;
	
	static AtomicInteger stamp = new AtomicInteger(1);
	public int stamp() {
		return stamp.getAndIncrement();
	}
	
	int[] consumed = new int[M];
	int[] produced = new int[M];

	class Producer implements Task<Void> {
		final int i;
		final Interval<Void> consumers[];
		final Interval<Void> prevConsumer;
		
		Producer(int i, Interval<Void>[] consumers,	Interval<Void> prevConsumer) {
			this.i = i;
			this.consumers = consumers;
			this.prevConsumer = prevConsumer;
		}
		
		public Integer produceData() { return i; }

		public Void run(Interval<Void> current) {
			if(i >= M) return null;
			produced[i] = stamp();
			
			Integer data = produceData();
			Consumer consTask = new Consumer(data);
			Interval<Void> consInter = intervalWithBound(current.bound())
			.startAfter(end(prevConsumer))
			.schedule(consTask);
			consumers[i % N] = consInter;
			
			int nxtI = (i+1) % N;
			Interval<Void> waitInter = consumers[nxtI]; 
				
			Producer nxtTask = new Producer(i + 1, consumers, consInter);
			Interval<?> nxtInter = intervalWithBound(current.bound())
			.startAfter(end(waitInter))
			.schedule(nxtTask);
			return null;
		}
		
		public String toString() {
			return String.format("Producer[%d]", i);
		}

	}
	
	class Consumer implements Task<Void> {
		public Integer i;
		
		private Consumer(Integer i) {
			this.i = i;
		}
		
		public String toString() {
			return String.format("Consumer[%d]", i);
		}

		public Void run(Interval<Void> current) {
			consumed[i] = stamp();
			return null;
		}
	}
	
	@Test public void test() {
//		ExecutionLog.enableGui();
		blockingInterval(new Task<Void>() {
			public Void run(Interval<Void> parent) {
				Producer fstProd = new Producer(0, new Interval[N], null);
				intervalDuring(parent).schedule(fstProd);
				return null;
			}			
		});
//		ExecutionLog.disable();
		
		for(int i = 1; i < M; i++)
			Assert.assertTrue(
					"i="+i+" consumed[i-1]="+consumed[i-1]+" consumed[i]="+consumed[i],
					consumed[i-1] < consumed[i]);
		for(int i = N; i < M; i++)
			Assert.assertTrue(
					"i="+i+" produced[i]="+produced[i]+" consumed[i-N]="+consumed[i-N],
					produced[i] > consumed[i-N]); 
	}

}
