package ch.ethz.intervals;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;

import org.junit.Assert;
import org.junit.Test;

import ch.ethz.intervals.impl.IntervalImpl;
import ch.ethz.intervals.impl.PointImpl;

public class TestFuzzyBarrier {
	
	final int N = 22; // number of workers
	final int MAX_COUNTER = 44;
	final Random R = new Random(66L);
	
	class Barrier extends SetupInterval {
		private IntervalImpl allRounds;
		private IntervalImpl thisRound;
		private IntervalImpl nextRound;
		int roundId = 0;
		final List<Integer> list = Collections.synchronizedList(new ArrayList<Integer>());
		int prevListSize = 0;
		
		public Barrier(@ParentForNew("Parent") Dependency dep) {
			super(dep, "barrier");
		}

		@Override
		public void setup(PointImpl currentEnd, IntervalImpl worker) {
			this.allRounds = worker;			
			startRound(new EmptyInterval(allRounds, "round0"));			
			for(int id = 0; id < N; id++)
				new WorkerTask(thisRound, id, R.nextInt(MAX_COUNTER));
		}
		
	    private void startRound(IntervalImpl inter) {
	        thisRound = inter;
	        IntervalImpl barrier = new BarrierTask(allRounds, ++roundId);
	        nextRound = new EmptyInterval(allRounds, "round"+roundId);
	       
	        // thisRound.end -> barrier -> nextRound.start
	        Intervals.addHb(thisRound.end, barrier.start);
	        Intervals.addHb(barrier.end, nextRound.start);
	    }
	    
	    class BarrierTask extends IntervalImpl {

			public BarrierTask(@ParentForNew("Parent") Dependency dep, int id) {
				super(dep, "barrier"+id);
			}

			@Override
			public void run() {
				System.err.println("Barrier");
				list.add(-1);
				if(list.size() > prevListSize + 1) {
					prevListSize = list.size();
					startRound(nextRound);
				}
			}
	    }
	    
	    class WorkerTask extends IntervalImpl {
	    	public final int id;
	    	public final int counter;
	    	
			public WorkerTask(IntervalImpl during, int id, int counter) {
				super(during, "worker["+id+"/"+roundId+"]");
				this.id = id;
				this.counter = counter;
			}

	    	class FuzzyTask extends IntervalImpl {
	    		
				public FuzzyTask(@ParentForNew("Parent") Dependency dep, WorkerTask nextWorker) {
					super(dep, "fuzzy"+id);
					Intervals.addHb(end, nextWorker.start);
				}

				@Override
				public void run() {
					System.err.println("Fuzzy Worker: "+id+" counter="+counter);
					list.add(0x80000000 | (id | (counter << 16)));
				}
	    		
	    	}
	    	
	    	@Override
			public void run() {
				System.err.println("Worker: "+id+" counter="+counter);
				list.add(id | (counter << 16));
				if(counter > 0) {
					new FuzzyTask(
							allRounds, 
							new WorkerTask(nextRound, id, counter-1));
				}
			}
	    }
	}
	
	@Test public void doTest() {
		Barrier b = Intervals.inline(new InlineTask<Barrier>() {
			@Override public String toString() { return "doTest"; }
			@Override public Barrier run(IntervalImpl subinterval) {
				return new Barrier(subinterval);
			}
		});
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
