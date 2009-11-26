package ch.ethz.intervals;

import static java.lang.Integer.valueOf;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import junit.framework.Assert;

import org.junit.Test;

public class TestFJ {

	final int N = 22;
	
	public @Test void testFJ() {
		final List<Integer> list = Collections.synchronizedList(new ArrayList<Integer>());

		// Interval task instance: defines the behavior of an interval,
		// like a Runnable.  This particular instance just adds the number
		// "i" to "list".
		class AddTask extends AbstractTask {
			final int i;
			
			public AddTask(int i) {
				this.i = i;
			}
			
			public void run(Point currentEnd) {
				list.add(i);
			}
		}
		
		// Create a new synchronous interval "current"
		// and spin off N independent children intervals,
		// each executing AddTask.  Synchronous intervals
		// are children of the current interval and the current
		// interval always waits for them to finish before 
		// proceeding.
		Intervals.blockingInterval(new Task() {
			public void run(Point parentEnd) {
				for(int i = 0; i < N; i++)
					Intervals.intervalWithBound(parentEnd).schedule(new AddTask(i));				
			}			
		});
		
		// At this point, all of those intervals should have finished! 
		list.add(-1);
		
		// Check that each interval executed once:
		Assert.assertEquals("Bad size", list.size(), N+1);		
		Set<Integer> seen = new HashSet<Integer>();
		for(int i : list.subList(0, N)) {
			Assert.assertTrue("Out of range", i >= 0 && i < N);
			Assert.assertTrue("Number appeared twice", seen.add(i));
		}		
		Assert.assertEquals("Does not end in -1", valueOf(-1), list.get(list.size() - 1));
		Assert.assertEquals("Not all numbers seen", N, seen.size());
	}

}
