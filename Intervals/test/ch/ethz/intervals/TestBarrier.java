package ch.ethz.intervals;

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

	class Barrier extends SetupInterval {
		private Interval parent;
		private Interval thisRound;
		private Interval nextRound;
		final List<Integer> list = Collections.synchronizedList(new ArrayList<Integer>());
		int prevListSize = 0;
		
		public Barrier(Dependency dep) {
			super(dep);
		}

		@Override
		public void setup(Point currentEnd, Interval worker) {
			this.parent = worker;			
			startRound(new EmptyInterval(parent, "round"));			
			for(int id = 0; id < N; id++)
				new WorkerTask(thisRound, id, R.nextInt(MAX_COUNTER));
		}
		
	    private void startRound(Interval inter) {
	        thisRound = inter;
	        Interval barrier = new BarrierTask(parent);
	        nextRound = new EmptyInterval(parent, "round");
	       
	        // thisRound.end -> barrier -> nextRound.start
	        Intervals.addHb(thisRound.end(), barrier.start());
	        Intervals.addHb(barrier.end(), nextRound.start());
	    }
	    
	    class BarrierTask extends Interval {

			public BarrierTask(Dependency dep) {
				super(dep);
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
	    
	    class WorkerTask extends Interval {
	    	public final int id;
	    	public final int counter;
	    	
			public WorkerTask(Interval during, int id, int counter) {
				super(during);
				this.id = id;
				this.counter = counter;
			}

			@Override
			public void run() {
				System.err.println("Worker: "+id+" counter="+counter);
				list.add(id | (counter << 16));
				if(counter > 0)
					new WorkerTask(nextRound, id, counter-1);
			}
	    }
	}
	
	@Test public void doTest() {
		Barrier b = 
			Intervals.blockingInterval(new Subinterval<Barrier>() {			
				public Barrier run(Interval subinterval) {
					return new Barrier(subinterval);
				}
			});
		
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
