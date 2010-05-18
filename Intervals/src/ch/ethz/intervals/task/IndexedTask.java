package ch.ethz.intervals.task;

import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.mirror.Interval;

/**
 * A more efficient way of creating {@code count} distinct
 * intervals and joining them immediately.  {@code task}
 * will be invoked with every integer from 0 to {@code count-1}.
 * Creates an outer interval which encompasses all
 * computations from 0 to {@code count-1}, but may create
 * any number of subintervals to do the actual computations
 * in parallel.  The task is always invoked with the end of the outer
 * interval, which is always an ancestor of the current interval
 * but may not be the current interval itself.  
 */
public abstract class IndexedTask extends AbstractTask {
	private final int lo0, hi0;
	private final int threshold;

	/** {@link #run(Interval, int, int)} will be invoked from all indices i where {@code 0 <= i < count} */
	public IndexedTask(int count) {
		this(0, count);
	}
	
	/** {@link #run(Interval, int, int)} will be invoked from all indices i where {@code lo <= i < hi} */
	public IndexedTask(int lo, int hi) {
		this.lo0 = lo;
		this.hi0 = hi;
		
		// By default, about 16 times as many tasks as threads.
		int count = hi - lo;
        int p = Intervals.context().getNumWorkers();
        this.threshold = (p > 1) ? (1 + count / (p << 4)) : count;		
	}
	
	public String toString() {
		return String.format("IndexedTask@%x(%d-%d)", System.identityHashCode(this), lo0, hi0);
	}
	
	abstract public void run(Interval current, int fromIndex, int toIndex);

	@Override
	public void run(Interval parent) {
		new Subtask(lo0, hi0).run(parent);
	}
	
	final class Subtask extends AbstractTask {
        final int lo;
        final int hi;
		
        Subtask(int lo, int hi) {
            this.lo = lo;
            this.hi = hi;
        }
        
        public String toString() {
        	return String.format("Subtask(%d-%d)", lo, hi);
        }

		@Override
		public void run(Interval current) {
            int l = lo;
            int h = hi;
            if (h - l > threshold) {
            	// Divvy up l-h into chunks smaller than threshold:
            	final int g = threshold;
                do {
                    int rh = h;
                    h = (l + h) >>> 1;
                    current.newAsyncChild(new Subtask(h, rh)).schedule();
                } while (h - l > g);
                
            }
            
            // Run what's left:
            IndexedTask.this.run(current, l, h);            	
		}		
	}

}