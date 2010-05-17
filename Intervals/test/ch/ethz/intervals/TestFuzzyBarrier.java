package ch.ethz.intervals;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;

import org.junit.Assert;
import org.junit.Test;

import ch.ethz.intervals.mirror.Interval;
import ch.ethz.intervals.task.AbstractTask;
import ch.ethz.intervals.task.EmptyTask;
import ch.ethz.intervals.task.ResultTask;
import ch.ethz.intervals.task.SetupTask;

public class TestFuzzyBarrier {
	
	final int N = 22; // number of workers
	final int MAX_COUNTER = 44;
	final Random R = new Random(66L);
	
	class Barrier extends SetupTask {
		private Interval allRounds;
		private Interval thisRound;
		private Interval nextRound;
		int roundId = 0;
		final List<Integer> list = Collections.synchronizedList(new ArrayList<Integer>());
		int prevListSize = 0;
		
		public Barrier() {
			super("barrier");
		}

		@Override
		public void setup(Interval setup, Interval worker) {
			this.allRounds = worker;			
			startRound(allRounds.newAsyncChild(new EmptyTask("round0")));			
			
			for(int id = 0; id < N; id++)
				thisRound.newAsyncChild(new WorkerTask(id, R.nextInt(MAX_COUNTER)));
		}
		
	    private void startRound(Interval inter) {
	        thisRound = inter;
	        Interval barrier = allRounds.newAsyncChild(new BarrierTask(++roundId));
	        nextRound = allRounds.newAsyncChild(new EmptyTask("round"+roundId));
	       
	        // thisRound.end -> barrier -> nextRound.start
	        Intervals.addHb(thisRound, barrier);
	        Intervals.addHb(barrier, nextRound);
	    }
	    
	    class BarrierTask extends AbstractTask {

			public BarrierTask(int id) {
				super("barrier"+id);
			}

			@Override
			public void run(Interval current) {
				System.err.println("Barrier");
				list.add(-1);
				if(list.size() > prevListSize + 1) {
					prevListSize = list.size();
					startRound(nextRound);
				}
			}
	    }
	    
	    class WorkerTask extends AbstractTask {
	    	public final int id;
	    	public final int counter;
	    	
			public WorkerTask(int id, int counter) {
				super("worker["+id+"/"+roundId+"]");
				this.id = id;
				this.counter = counter;
			}
			
			class FuzzyTask extends AbstractTask {
	    		
				public FuzzyTask() {
					super("fuzzy"+id);
				}

				@Override
				public void run(Interval current) {
					System.err.println("Fuzzy Worker: "+id+" counter="+counter);
					list.add(0x80000000 | (id | (counter << 16)));
				}
	    		
	    	}
			
	    	@Override
			public void run(Interval current) {
				System.err.println("Worker: "+id+" counter="+counter);
				list.add(id | (counter << 16));
				if(counter > 0) {
					Interval nextWorker = nextRound.newAsyncChild(new WorkerTask(id, counter - 1));
					Interval fuzzyTask = allRounds.newAsyncChild(new FuzzyTask());
					Intervals.addHb(fuzzyTask, nextWorker);
				}
			}
	    }
	}
	
	@Test public void doTest() {
		Barrier b = Intervals.inline(new ResultTask<Barrier>("doTest") {
			@Override protected Barrier compute(Interval subinterval) {
				Barrier barrier = new Barrier();
				subinterval.newAsyncChild(barrier);
				return barrier;
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
