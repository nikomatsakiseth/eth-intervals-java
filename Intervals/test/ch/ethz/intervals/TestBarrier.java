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

public class TestBarrier {
	
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
	    
	    void barrier(Task afterBarrier) {
	    	intervalDuring(nextRound, afterBarrier);
	    }
	    
	    class WorkerTask extends AbstractTask {
	    	public final int id;
	    	public final int counter;
	    	
			public WorkerTask(int id, int counter) {
				this.id = id;
				this.counter = counter;
			}

			@Override
			public void run(Point currentEnd) {
				System.err.println("Worker: "+id+" counter="+counter);
				list.add(id | (counter << 16));
				if(counter > 0)
					barrier(new WorkerTask(id, counter-1));
			}
	    }
	}
	
	@Test public void doTest() {
		Barrier b = new Barrier();
		Intervals.blockingInterval(b);
		
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
