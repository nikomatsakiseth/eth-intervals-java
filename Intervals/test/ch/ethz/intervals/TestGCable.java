package ch.ethz.intervals;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;

import ch.ethz.intervals.impl.IntervalImpl;
import ch.ethz.intervals.impl.PointImpl;

/**
 * Tests which show that subintervals are gc-able, even if you hold
 * on to the super interval or to the points within.
 */
public class TestGCable {

	@Test public void subintervalsGcable() {		
		final List<WeakReference<IntervalImpl>> list = 
			new ArrayList<WeakReference<IntervalImpl>>();
		
		for(int i = 0; i < 10; i++)
			Intervals.inline(new VoidInlineTask() {			
				@Override public void run(IntervalImpl subinterval) {
					list.add(new WeakReference<IntervalImpl>(subinterval));
				}
			});
		
		runGc();
		
		for(WeakReference<IntervalImpl> weakRef : list)
			Assert.assertTrue(weakRef.get() == null);
	}

	private void runGc() {
		for(int i = 0; i < 3; i++)
			System.gc();
	}
	
	@Test public void asyncIntervalsGcable() {		
		final List<IntervalImpl> realRefs = new ArrayList<IntervalImpl>();
		final List<PointImpl> allPoints = new ArrayList<PointImpl>();
		
		final List<WeakReference<IntervalImpl>> list = 
			new ArrayList<WeakReference<IntervalImpl>>();
		
		for(int i = 0; i < 10; i++)
			// Note: the subintervals stay live, but not the async intervals contained
			// within!
			Intervals.inline(new VoidInlineTask() {			
				@Override public void run(IntervalImpl subinterval) {					
					allPoints.add(subinterval.start);
					allPoints.add(subinterval.end);
					
					realRefs.add(subinterval);
					list.add(new WeakReference<IntervalImpl>(subinterval));
					
					for(int j = 0; j < 10; j++) {
						IntervalImpl inter = new EmptyInterval(subinterval, "Empty:"+j);
						allPoints.add(inter.start);
						allPoints.add(inter.end);
						
						inter.schedule();
						list.add(new WeakReference<IntervalImpl>(inter));
					}
				}
			});
		
		runGc();
		
		for(WeakReference<IntervalImpl> weakRef : list)
			Assert.assertTrue(weakRef.get() == null || realRefs.contains(weakRef.get()));
	}
	
}
