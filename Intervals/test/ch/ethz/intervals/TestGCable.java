package ch.ethz.intervals;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;

import ch.ethz.intervals.mirror.AsyncInterval;
import ch.ethz.intervals.mirror.Interval;
import ch.ethz.intervals.mirror.Point;
import ch.ethz.intervals.task.AbstractTask;
import ch.ethz.intervals.task.EmptyTask;

/**
 * Tests which show that subintervals are gc-able, even if you hold
 * on to the super interval or to the points within.
 */
public class TestGCable {

	@Test public void subintervalsGcable() {		
		final List<WeakReference<Interval>> list = 
			new ArrayList<WeakReference<Interval>>();
		
		for(int i = 0; i < 10; i++)
			Intervals.inline(new AbstractTask() {			
				@Override public void run(Interval subinterval) {
					list.add(new WeakReference<Interval>(subinterval));
				}
			});
		
		runGc();
		
		for(WeakReference<Interval> weakRef : list)
			Assert.assertTrue(weakRef.get() == null);
	}

	private void runGc() {
		for(int i = 0; i < 3; i++)
			System.gc();
	}
	
	@Test public void asyncIntervalsGcable() {		
		final List<Interval> realRefs = new ArrayList<Interval>();
		final List<Point> allPoints = new ArrayList<Point>();
		
		final List<WeakReference<Interval>> list = 
			new ArrayList<WeakReference<Interval>>();
		
		for(int i = 0; i < 10; i++)
			// Note: the subintervals stay live, but not the async intervals contained
			// within!
			Intervals.inline(new AbstractTask() {			
				@Override public void run(Interval subinterval) {					
					allPoints.add(subinterval.getStart());
					allPoints.add(subinterval.getEnd());
					
					realRefs.add(subinterval);
					list.add(new WeakReference<Interval>(subinterval));
					
					for(int j = 0; j < 10; j++) {
						AsyncInterval inter = subinterval.newAsyncChild(new EmptyTask("Empty:"+j));
						allPoints.add(inter.getStart());
						allPoints.add(inter.getEnd());
						
						inter.schedule();
						list.add(new WeakReference<Interval>(inter));
					}
				}
			});
		
		runGc();
		
		for(WeakReference<Interval> weakRef : list)
			Assert.assertTrue(weakRef.get() == null || realRefs.contains(weakRef.get()));
	}
	
}
