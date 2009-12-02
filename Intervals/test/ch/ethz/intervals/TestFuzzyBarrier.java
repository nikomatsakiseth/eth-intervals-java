package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.emptyTask;
import static ch.ethz.intervals.Intervals.intervalDuring;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;

import org.junit.Assert;
import org.junit.Test;

public class TestFuzzyBarrier {
	
	final int N = 22; // number of workers
	final int MAX_COUNTER = 44;
	final Random R = new Random(66L);

	class Barrier extends SetupTask {
		private Interval parent;
		private Interval thisRound;
		private Interval nextRound;
		final List<Integer> list = Collections.synchronizedList(new ArrayList<Integer>());
		int prevListSize = 0;
		
		@Override
		public void setup(Point currentEnd, Interval worker) {
			this.parent = worker;			
			startRound(intervalDuring(worker, emptyTask));			
			for(int id = 0; id < N; id++)
				intervalDuring(thisRound, new WorkerTask(id, R.nextInt(MAX_COUNTER)));
		}
		
	    private void startRound(Interval inter) {
	        thisRound = inter;
	        Interval barrier = intervalDuring(parent, new BarrierTask());
	        nextRound = intervalDuring(parent, emptyTask);
	       
	        // thisRound.end -> barrier -> nextRound.start
	        Intervals.addHb(thisRound.end(), barrier.start());
	        Intervals.addHb(barrier.end(), nextRound.start());
	    }
	    
	    class BarrierTask extends AbstractTask {
			@Override
			public void run(Point currentEnd) {
				System.err.println("Barrier");
				list.add(-1);
				if(list.size() > prevListSize + 1) {
					prevListSize = list.size();
					startRound(nextRound);
				}
			}
	    }
	    final BarrierTask barrierTask = new BarrierTask();
	    
	    void barrier(Task untilBarrier, Task afterBarrier) {
	    	Interval fuzzy = intervalDuring(parent, untilBarrier); // overlaps with barrier!
	    	Interval strict = intervalDuring(nextRound, afterBarrier);
	    	Intervals.addHb(fuzzy.end(), strict.start());
	    }
	    
	    class WorkerTask extends AbstractTask {
	    	public final int id;
	    	public final int counter;
	    	
	    	class FuzzyTask extends AbstractTask {

				@Override
				public void run(Point currentEnd) {
					System.err.println("Fuzzy Worker: "+id+" counter="+counter);
					list.add(0x80000000 | (id | (counter << 16)));
				}
	    		
	    	}
	    	
			public WorkerTask(int id, int counter) {
				this.id = id;
				this.counter = counter;
			}

			@Override
			public void run(Point currentEnd) {
				System.err.println("Worker: "+id+" counter="+counter);
				list.add(id | (counter << 16));
				if(counter > 0)
					barrier(new FuzzyTask(), new WorkerTask(id, counter-1));
			}
	    }
	}
	
	@Test public void doTest() {
		Barrier b = new Barrier();
		Intervals.blockingInterval(b);
		System.err.println();
		System.err.println();
		System.err.println();

		Set<Integer> expected = new HashSet<Integer>();
		for(int n = 0; n < N; n++) expected.add(n);
		
		Set<Integer> found = new HashSet<Integer>();
		Set<Integer> fuzzy = new HashSet<Integer>(expected);
		for(int l : b.list) {
			final int id = (l & 0xFFFF);
			if(l == -1) {
				System.err.println("Clear round");
				Assert.assertEquals("Not all workers finished before barrier fired", expected, found);
				found.clear();
			} else if (l < 0) { // Fuzzy result
				System.err.println("Fuzzy id="+id+" counter="+((l >> 16) & 0x7FFF));
				Assert.assertTrue("Two fuzzies for "+id, fuzzy.add(id));
			} else if (l < N) { // counter == 0 -> don't expect worker on later rounds
				System.err.println("Worker done id="+id);
				Assert.assertTrue("Worker finished twice", expected.remove(l));
			} else { // Countdown result: should have seen fuzzy in previous round
				System.err.println("Worker id="+id+" counter="+(l >> 16));
				Assert.assertTrue("No fuzzy for "+id, fuzzy.remove(id));
				Assert.assertTrue("Worker appeared twice in one round", found.add(id));
			}
		}
		
		Assert.assertTrue("Last round did not end", found.isEmpty());
		Assert.assertTrue("Not all workers finished", expected.size() == 0);
	}
	
}
