package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.addHb;
import static ch.ethz.intervals.Intervals.subinterval;
import static ch.ethz.intervals.Intervals.child;
import static ch.ethz.intervals.Intervals.successor;

import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Assert;
import org.junit.Test;

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
		private final DynamicGuard dg = new DynamicGuard();
		private Interval interval;
		
		public Interval get() {
			dg.checkReadable();
			return interval;
		}
		
		public void set(Interval interval) {
			dg.checkWritable();
			this.interval = interval;
		}
	}

	public class BBPC extends Interval {

		public BBPC(Dependency dep) {
			super(dep);
			
			for(int i = 0; i < N; i++) {
				producers[i] = new BoxedInterval();
				consumers[i] = new BoxedInterval();
			}
		}

		final int N = 3;
		final BoxedInterval[] producers = new BoxedInterval[N];
		final BoxedInterval[] consumers = new BoxedInterval[N];

		class Producer extends Interval {
			final int index;

			public Producer(Dependency dep, int index) {
				super(dep);
				this.index = index;
				
				// and the consumer N indices before:
				if (index >= N)
					addHb(consumers[(index - N) % N].get().end, start);
			}

			public String toString() {
				return "Producer["+index+"]";
			}

			public void run() {
				produced[index] = (index * 2);
				if(index + 1 < M)
					producers[(index + 1) % N].set(new Producer(successor(), index + 1));
				else
					producers[(index + 1) % N].set(null);
			}
		}

		class Consumer extends Interval {
			final int index;
			
			public Consumer(Dependency dep, int index) {
				super(dep);
				this.index = index;

				// comes after the producer:
				addHb(producers[index % N].get().end, start);
			}
			
			public String toString() {
				return "Consumer["+index+"]";
			}

			public void run() {
				consumed[index] = produced[index];
				if(producers[(index + 1) % N].get() != null)
					consumers[(index + 1) % N].set(new Consumer(successor(), index + 1));
			}
		}

		public void run() {
			Intervals.subinterval(new VoidSubinterval() {
				@Override public void run(Interval init) {
					producers[0].set(new Producer(successor(), 0));
					consumers[0].set(new Consumer(successor(), 0));
				}
			});
		}

	}

	@Test
	public void test() {
		subinterval(new VoidSubinterval() {
			@Override public void run(final Interval subinterval) {
				new BBPC(subinterval);
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
