package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.emptyTask;
import static ch.ethz.intervals.Intervals.end;
import static ch.ethz.intervals.Intervals.intervalDuring;
import static ch.ethz.intervals.Intervals.start;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;

import org.junit.Assert;
import org.junit.Test;

public class TestBarrier {
	
	final int N = 22; // number of workers
	final int MAX_COUNTER = 44;
	final Random R = new Random(66L);

	class Barrier implements SetupTask<Void> {
		private Interval<Void> parent;
		private Interval<Void> thisRound;
		private Interval<Void> nextRound;
		final List<Integer> list = Collections.synchronizedList(new ArrayList<Integer>());
		int prevListSize = 0;
		
		@Override
		public Void setup(Interval<Void> current, Interval<Void> worker) {
			this.parent = worker;			
			startRound(intervalDuring(worker).schedule(emptyTask));			
			for(int id = 0; id < N; id++)
				intervalDuring(thisRound).schedule(new WorkerTask(id, R.nextInt(MAX_COUNTER)));
			return null;
		}
		
	    private void startRound(Interval<Void> inter) {
	        thisRound = inter;
	        Interval<Void> barrier = intervalDuring(parent).startAfter(end(thisRound)).schedule(new BarrierTask());
	        nextRound = intervalDuring(parent).startAfter(end(barrier)).schedule(emptyTask);
	    }
	    
	    class BarrierTask implements Task<Void> {
			@Override
			public Void run(Interval<Void> current) {
				System.err.println("Barrier");
				list.add(-1);
				if(list.size() > prevListSize + 1) {
					prevListSize = list.size();
					startRound(nextRound);
				}
				return null;
			}
	    }
	    final BarrierTask barrierTask = new BarrierTask();
	    
	    void barrier(Task<?> afterBarrier) {
	    	intervalDuring(nextRound).schedule(afterBarrier);
	    }
	    
	    class WorkerTask implements Task<Void> {
	    	public final int id;
	    	public final int counter;
	    	
			public WorkerTask(int id, int counter) {
				this.id = id;
				this.counter = counter;
			}

			@Override
			public Void run(Interval<Void> current) {
				System.err.println("Worker: "+id+" counter="+counter);
				list.add(id | (counter << 16));
				if(counter > 0)
					barrier(new WorkerTask(id, counter-1));
				return null;
			}
	    }
	}
	
	@Test public void doTest() {
		Barrier b = new Barrier();
		Intervals.blockingSetupInterval(b);
		
		Set<Integer> expected = new HashSet<Integer>();
		for(int n = 0; n < N; n++) expected.add(n);
		
		Set<Integer> found = new HashSet<Integer>();
		for(Integer l : b.list) {
			if(l == -1L) {
				Assert.assertEquals("Not all workers finished before barrier fired", expected, found);
				found.clear();
			} else if (l < N) {
				Assert.assertTrue("Worker finished twice", expected.remove(l));
			} else {
				Assert.assertTrue("Worker appeared twice in one round", found.add(l & 0xFFFF));				
			}
		}
		
		Assert.assertTrue("Last round did not end", found.isEmpty());
		Assert.assertTrue("Not all workers finished", expected.size() == 0);
	}
	
}
