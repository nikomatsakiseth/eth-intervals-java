package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.addHb;
import static ch.ethz.intervals.Intervals.inline;

import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Assert;
import org.junit.Test;

import ch.ethz.intervals.impl.IntervalImpl;
import ch.ethz.intervals.task.AbstractTask;

/**
 * Bounded-Buffer Producer Consumer example using arrays.
 */
public class TestBBPCArray {

	public final int N = 3;
	public final int M = 100;

	static AtomicInteger stamp = new AtomicInteger(1);

	public int stamp() {
		return stamp.getAndIncrement();
	}

	int[] consumed = new int[M];
	int[] produced = new int[M];

	public class BBPC extends AbstractTask {

		public BBPC() {
			super("BBPC");
		}

		final int N = 3;
		final Interval[] producers = new IntervalImpl[N];
		final Interval[] consumers = new IntervalImpl[N];

		public Interval newProducer(Interval parent, int index) {
			Interval inter = parent.newAsyncChild(new Producer(index)); 
			
			// comes after the previous producer:
			if (index > 0)
				addHb(producers[(index - 1) % N], inter);
			
			// and the consumer N indices before:
			if (index >= N)
				addHb(consumers[(index - N) % N], inter);
			
			return inter;
		}

		class Producer extends AbstractTask {
			final int index;

			public Producer(int index) {
				super("p"+index);
				this.index = index;
			}
			
			public void run(Interval current) {
				produced[index] = (index * 2);
				if(index + 1 < M)
					producers[(index + 1) % N] = newProducer(current.getParent(), index + 1);
				else
					producers[(index + 1) % N] = null;
			}
		}

		public Interval newConsumer(Interval parent, int index) {
			Interval inter = parent.newAsyncChild(new Consumer(index));
			
			// comes after the producer:
			addHb(producers[index % N], inter);
			
			// and after the previous consumer:
			if (index > 0)
				addHb(consumers[(index - 1) % N], inter);
			
			return inter;
		}
		
		class Consumer extends AbstractTask {
			final int index;
			
			public Consumer(int index) {
				super("c"+index);
				this.index = index;
			}
			
			public void run(Interval current) {
				consumed[index] = produced[index];
				if(producers[(index + 1) % N] != null)
					consumers[(index + 1) % N] = newConsumer(current.getParent(), index + 1);
			}
		}

		public void run(Interval current) {
			producers[0] = newProducer(current, 0);
			consumers[0] = newConsumer(current, 0);
		}

	}

	@Test
	public void test() {
		inline(new AbstractTask() {
			@Override
			public void run(Interval subinterval) {
				subinterval.newAsyncChild(new BBPC());
			}
		});

		for (int i = 1; i < M; i++)
			Assert.assertTrue("i=" + i + " consumed[i-1]=" + consumed[i - 1]
					+ " consumed[i]=" + consumed[i],
					consumed[i - 1] < consumed[i]);
		for (int i = N; i < M; i++)
			Assert.assertTrue("i=" + i + " produced[i]=" + produced[i]
					+ " consumed[i-N]=" + consumed[i - N],
					produced[i] > consumed[i - N]);
	}

}
