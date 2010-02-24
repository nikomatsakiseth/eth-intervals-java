package ch.ethz.intervals;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;

/**
 * Tests which show that subintervals are gc-able, even if you hold
 * on to the super interval or to the points within.
 */
public class TestGCable {

	@Test public void subintervalsGcable() {		
		final List<WeakReference<Interval>> list = 
			new ArrayList<WeakReference<Interval>>();
		
		for(int i = 0; i < 10; i++)
			Intervals.inline(new VoidInlineTask() {			
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
			Intervals.inline(new VoidInlineTask() {			
				@Override public void run(Interval subinterval) {					
					allPoints.add(subinterval.start);
					allPoints.add(subinterval.end);
					
					realRefs.add(subinterval);
					list.add(new WeakReference<Interval>(subinterval));
					
					for(int j = 0; j < 10; j++) {
						Interval inter = new EmptyInterval(subinterval, "Empty:"+j);
						allPoints.add(inter.start);
						allPoints.add(inter.end);
						
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
