package ch.ethz.intervals;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;

import org.junit.Assert;
import org.junit.Test;

import ch.ethz.intervals.impl.ContextImpl;
import ch.ethz.intervals.impl.PointImpl;
import ch.ethz.intervals.task.AbstractTask;
import ch.ethz.intervals.task.EmptyTask;
import ch.ethz.intervals.task.ResultTask;
import ch.ethz.intervals.task.SetupTask;

public class TestBarrier {
	
	final int N = 22; // number of workers
	final int MAX_COUNTER = 44;
	final Random R = new Random(66L);
	
	public void println(String s) {
		//System.err.println(s);
	}

	class Barrier extends SetupTask {
		private Interval parent;
		private Interval thisRound;
		private Interval nextRound;
		final List<Integer> list = Collections.synchronizedList(new ArrayList<Integer>());
		int prevListSize = 0;
		
		Barrier() {
			super("Barrier");
		}

		@Override
		public void setup(Interval setup, Interval worker) {
			this.parent = worker;			
			startRound(parent.newAsyncChild(new EmptyTask("round-1")), 0);			
			for(int id = 0; id < N; id++)
				thisRound.newAsyncChild(new WorkerTask(id, R.nextInt(MAX_COUNTER)));
		}
		
	    private void startRound(Interval inter, int id) {
	        thisRound = inter;
	        Interval barrier = parent.newAsyncChild(new BarrierTask(id));
	        nextRound = parent.newAsyncChild(new EmptyTask("round" + id));
	       
	        // thisRound.end -> barrier -> nextRound.start
	        Intervals.addHb(thisRound, barrier);
	        Intervals.addHb(barrier, nextRound);
	    }
	    
	    class BarrierTask extends AbstractTask {
	    	final int prevRoundId;
	    	
	    	BarrierTask(int prevRoundId) {
	    		super("Barrier" + prevRoundId);
	    		this.prevRoundId = prevRoundId;
	    	}
	    	
			@Override
			public void run(Interval current) {
				println(toString());
				list.add(-1);
				if(list.size() > prevListSize + 1) {
					prevListSize = list.size();
					startRound(nextRound, prevRoundId + 1);
				}
			}
			
	    }
	    
	    class WorkerTask extends AbstractTask {
	    	public final int id;
	    	public final int counter;
	    	
			public WorkerTask(int id, int counter) {
				super("Worker-"+id+"-"+counter);
				this.id = id;
				this.counter = counter;
			}

			@Override
			public void run(Interval current) {
				println(toString());
				list.add(id | (counter << 16));
				if(counter > 0)
					nextRound.newAsyncChild(new WorkerTask(id, counter-1));
			}
	    }
	}
	
	@Test public void doTest() {
		//ExecutionLog.enableGui();
		Barrier b;
		try {
			b = Intervals.inline(new ResultTask<Barrier>() {			
					@Override public Barrier compute(Interval subinterval) {
						Barrier barrier = new Barrier();
						subinterval.newAsyncChild(barrier);
						return barrier;
					}
				});
		} finally {
			//ExecutionLog.disable();
		}
		
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
