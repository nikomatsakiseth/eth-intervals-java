package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.addHb;
import static ch.ethz.intervals.Intervals.blockingInterval;

import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Assert;
import org.junit.Test;

/**
 * Bounded-Buffer Producer Consumer example
 * using arrays.
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

	public class BBPC extends AbstractTask {

		final int N = 3;
		final Interval[] producers = new Interval[N];
		final Interval[] consumers = new Interval[N];

		class Producer extends AbstractTask {
			final int index;

			public Producer(int index) {
				this.index = index;
			}

			public String toString() {
				return "Producer["+index+"]";
			}

			public void addDependencies(Interval inter) {
				// comes after the previous producer:
				if (index > 0)
					addHb(producers[(index - 1) % N].end(), inter.start());
				
				// and the consumer N indices before:
				if (index >= N)
					addHb(consumers[(index - N) % N].end(), inter.start());
			}

			public void run(Point end) {
				produced[index] = (index * 2);
				if(index + 1 < M)
					producers[(index + 1) % N] = 
						Intervals.siblingInterval(new Producer(index + 1));
				else
					producers[(index + 1) % N] = null;
			}
		}

		class Consumer extends AbstractTask {
			final int index;
			
			public Consumer(int index) {
				this.index = index;
			}
			
			public String toString() {
				return "Consumer["+index+"]";
			}

			public void addDependencies(Interval inter) {
				// comes after the producer:
				addHb(producers[index % N].end(), inter.start());
				
				// and after the previous consumer:
				if (index > 0)
					addHb(consumers[(index - 1) % N].end(), inter.start());
			}

			public void run(Point end) {
				consumed[index] = produced[index];
				if(producers[(index + 1) % N] != null)
					consumers[(index + 1) % N] = 
						Intervals.siblingInterval(new Consumer(index + 1));
			}
		}

		public void run(Point end) {
			producers[0] = Intervals.childInterval(new Producer(0));
			consumers[0] = Intervals.childInterval(new Consumer(0));
		}

	}

	@Test
	public void test() {
		blockingInterval(new BBPC());

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