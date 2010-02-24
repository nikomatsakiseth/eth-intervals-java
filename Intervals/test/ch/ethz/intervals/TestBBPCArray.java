package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.addHb;
import static ch.ethz.intervals.Intervals.inline;
import static ch.ethz.intervals.Intervals.child;
import static ch.ethz.intervals.Intervals.successor;

import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Assert;
import org.junit.Test;

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

	public class BBPC extends Interval {

		public BBPC(Dependency dep) {
			super(dep);
		}

		final int N = 3;
		final Interval[] producers = new Interval[N];
		final Interval[] consumers = new Interval[N];

		class Producer extends Interval {
			final int index;

			public Producer(Dependency dep, int index) {
				super(dep);
				this.index = index;
				
				// comes after the previous producer:
				if (index > 0)
					addHb(producers[(index - 1) % N].end, start);
				
				// and the consumer N indices before:
				if (index >= N)
					addHb(consumers[(index - N) % N].end, start);
			}

			public String toString() {
				return "Producer["+index+"]";
			}

			public void run() {
				produced[index] = (index * 2);
				if(index + 1 < M)
					producers[(index + 1) % N] = new Producer(successor(), index + 1);
				else
					producers[(index + 1) % N] = null;
			}
		}

		class Consumer extends Interval {
			final int index;
			
			public Consumer(Dependency dep, int index) {
				super(dep);
				this.index = index;

				// comes after the producer:
				addHb(producers[index % N].end, start);
				
				// and after the previous consumer:
				if (index > 0)
					addHb(consumers[(index - 1) % N].end, start);
			}
			
			public String toString() {
				return "Consumer["+index+"]";
			}

			public void run() {
				consumed[index] = produced[index];
				if(producers[(index + 1) % N] != null)
					consumers[(index + 1) % N] = new Consumer(successor(), index + 1);
			}
		}

		public void run() {
			producers[0] = new Producer(this, 0);
			consumers[0] = new Consumer(this, 0);
		}

	}

	@Test
	public void test() {
		inline(new VoidInlineTask() {
			public void run(Interval subinterval) {
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
