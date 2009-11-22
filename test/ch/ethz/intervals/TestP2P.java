package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.end;
import static ch.ethz.intervals.Intervals.intervalDuring;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import junit.framework.Assert;

import org.junit.Test;

public class TestP2P {
	
	final int N = 22;
	final int M = 44;
	
	public static int c(int n, int m) {
		return n | (m << 16);
	}
	
	class P2P implements SetupTask<Void> {
		final List<Integer> list = Collections.synchronizedList(new ArrayList<Integer>());
		
	    @SuppressWarnings("unchecked")
	    @Override
		public Void setup(Interval<Void> _, Interval<Void> parent) {        
	        Interval<Void>[][] intervals = new Interval[2][M+2];
	        
	        for(int n = 0; n < N; n++) {
	            int bit = n % 2, prevBit = 1 - bit;
	            for(int m = 1; m < M+1; m++) {
	                intervals[bit][m] = intervalDuring(parent)
	                	.startAfter(end(intervals[prevBit][m-1]))
	                	.startAfter(end(intervals[prevBit][m]))
	                	.startAfter(end(intervals[prevBit][m+1]))
	                	.schedule(new AddTask(n, m-1));
	            }
	        }   
	        
	        return null;
	    }

		class AddTask implements Task<Void> {
			
			final int n, m;
			
			public AddTask(int n, int m) {
				this.n = n;
				this.m = m;
			}

			@Override
			public Void run(Interval<Void> current) {
				list.add(c(n, m));
				return null;
			}
			
		}


	}
	
	@Test public void testP2P() {
		P2P p2p = new P2P();		
		Intervals.blockingSetupInterval(p2p);
		
		Set<Integer> finished = new HashSet<Integer>();
		for(Integer i : p2p.list) {
			int n = i & 0xFFFF;
			int m = i >> 16;
//	        System.err.printf("i=0x%x n=0x%x m=0x%x\n", i, n, m);
			if (n > 0) {
				if(m > 0)
					Assert.assertTrue("Precondition not finished", finished.contains(c(n-1, m-1)));
				Assert.assertTrue("Precondition not finished", finished.contains(c(n-1, m)));
				if(m < M-1)
					Assert.assertTrue("Precondition not finished", finished.contains(c(n-1, m+1)));
			}			
			Assert.assertTrue("Finished twice!", finished.add(i));
		}
		
		for(int n = 0; n < N; n++)
			for(int m = 0; m < M; m++)
				Assert.assertTrue("Square never finished", finished.contains(c(n, m)));
	}
	
}
