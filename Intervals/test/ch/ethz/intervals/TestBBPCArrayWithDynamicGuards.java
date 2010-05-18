package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.addHb;
import static ch.ethz.intervals.Intervals.inline;

import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Assert;
import org.junit.Test;

import ch.ethz.intervals.guard.ReadTrackingDynamicGuard;
import ch.ethz.intervals.task.AbstractTask;

/**
 * Bounded-Buffer Producer Consumer example using arrays.
 */
public class TestBBPCArrayWithDynamicGuards {

	public final int N = 3;
	public final int M = 100;

	static AtomicInteger stamp = new AtomicInteger(1);

	public int stamp() {
		return stamp.getAndIncrement();
	}

	int[] consumed = new int[M];
	int[] produced = new int[M];
	
	public class BoxedInterval {
		private final ReadTrackingDynamicGuard dg = new ReadTrackingDynamicGuard();
		private Interval intervalImpl;
		
		public Interval get() {
			Intervals.checkReadable(dg);
			return intervalImpl;
		}
		
		public void set(Interval intervalImpl) {
			Intervals.checkWritable(dg);
			this.intervalImpl = intervalImpl;
		}
	}

	public class BBPC extends AbstractTask {

		public BBPC() {
			super("BBPC");
			
			for(int i = 0; i < N; i++) {
				producers[i] = new BoxedInterval();
				consumers[i] = new BoxedInterval();
			}
		}

		final int N = 3;
		final BoxedInterval[] producers = new BoxedInterval[N];
		final BoxedInterval[] consumers = new BoxedInterval[N];
		
		public Interval newProducer(Interval parent, int index) {
			Interval inter = parent.newAsyncChild(new Producer(index));
			
			// after the consumer N indices before:
			if (index >= N)
				addHb(consumers[(index - N) % N].get(), inter);
			
			return inter;
		}

		class Producer extends AbstractTask {
			final int index;

			public Producer(int index) {
				super("p"+index);
				this.index = index;
			}

			@Override
			public void run(Interval current) {
				produced[index] = (index * 2);
				if(index + 1 < M)
					producers[(index + 1) % N].set(newProducer(current.getParent(), index + 1));
				else
					producers[(index + 1) % N].set(null);
			}
		}

		public Interval newConsumer(Interval parent, int index) {
			Interval inter = parent.newAsyncChild(new Consumer(index));

			// comes after the producer:
			addHb(producers[index % N].get(), inter);
			
			return inter;
		}

		class Consumer extends AbstractTask {
			final int index;
			
			public Consumer(int index) {
				super("c"+index);
				this.index = index;
			}
			
			public String toString() {
				return "Consumer["+index+"]";
			}

			@Override
			public void run(Interval current) {
				consumed[index] = produced[index];
				if(producers[(index + 1) % N].get() != null)
					consumers[(index + 1) % N].set(newConsumer(current.getParent(), index + 1));
			}
		}

		public void run(Interval current) {
			producers[0].set(newProducer(current, 0));
			consumers[0].set(newConsumer(current, 0));
		}

	}

	@Test
	public void test() {
		inline(new AbstractTask() {
			@Override public void run(final Interval subinterval) {
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
