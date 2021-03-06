package ch.ethz.intervals.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import junit.framework.Assert;

import org.junit.Test;

import ch.ethz.intervals.AsyncInterval;
import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.Point;
import ch.ethz.intervals.task.AbstractTask;
import ch.ethz.intervals.task.ResultTask;
import ch.ethz.intervals.task.SetupTask;

public class TestP2P {
	
	final int N = 22;
	final int M = 44;
	
	public static int c(int n, int m) {
		return n | (m << 16);
	}
	
	class P2P extends SetupTask {
		final List<Integer> list = Collections.synchronizedList(new ArrayList<Integer>());
		
	    @Override
		public void setup(Interval _, Interval parent) {        
	        Point[][] intervals = new Point[2][M+2];
	        
	        for(int n = 0; n < N; n++) {
	            int bit = n % 2, prevBit = 1 - bit;
	            for(int m = 1; m < M+1; m++) {
	            	AsyncInterval i = parent.newAsyncChild(new AddTask(n, m-1));
	                intervals[bit][m] = i.getEnd(); 
	                if(intervals[prevBit][m-1] != null)
	                	Intervals.addHb(intervals[prevBit][m-1], i.getStart());
	                if(intervals[prevBit][m] != null)
	                	Intervals.addHb(intervals[prevBit][m], i.getStart());
	                if(intervals[prevBit][m+1] != null)
	                	Intervals.addHb(intervals[prevBit][m+1], i.getStart());
	                i.schedule();
	            }
	        }   
	    }

		class AddTask extends AbstractTask {
			
			final int n, m;
			
			public AddTask(int n, int m) {
				super("Add("+n+","+m+")");
				this.n = n;
				this.m = m;
			}

			@Override
			public void run(Interval _) {
				list.add(c(n, m));
			}
			
		}


	}
	
	@Test public void testP2P() {
		P2P p2p = Intervals.inline(new ResultTask<P2P>() {
			public P2P compute(Interval subinterval) {
				P2P res = new P2P();
				subinterval.newAsyncChild(res);
				return res;
			}
		});
		
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
