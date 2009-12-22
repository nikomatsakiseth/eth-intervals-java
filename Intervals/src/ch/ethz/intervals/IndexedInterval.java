package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.POOL;

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
 * 
 * Because the current interval is not predictable in the
 * {@link #run(Point, int, int)} method, it is not recommended
 * to use {@link Intervals#siblingInterval(Task)} or 
 * {@link Intervals#successorInterval(Task)}, although 
 * {@link Intervals#childInterval(Task)} will still lead to the
 * desired results.
 * 
 * @param count the number of times to invoke {@code task}
 * @param task the task to invoke
 */
public abstract class IndexedInterval extends Interval {
	private final int lo0, hi0;
	private final int threshold;
	private Point parentEnd;

	/** {@link #run(Point, int, int)} will be invoked from all indices i where {@code 0 <= i < count} */
	public IndexedInterval(Point bound, int count) {
		this(bound, 0, count);
	}
	
	/** {@link #run(Point, int, int)} will be invoked from all indices i where {@code lo <= i < hi} */
	public IndexedInterval(Point bound, int lo, int hi) {
		super(bound);
		this.lo0 = lo;
		this.hi0 = hi;
		
		// By default, about 16 times as many tasks as threads.
		int count = hi - lo;
        int p = POOL.numWorkers;
        this.threshold = (p > 1) ? (1 + count / (p << 4)) : count;		
	}
	
	public String toString() {
		return String.format("IndexedTask@%x(%d-%d)", System.identityHashCode(this), lo0, hi0);
	}
	
	abstract public void run(Point parentEnd, int fromIndex, int toIndex);

	@Override
	public final void run() {
		parentEnd = end();
    	new Subtask(parentEnd, lo0, hi0);
	}
	
	final class Subtask extends Interval {
        final int lo;
        final int hi;
		
        Subtask(Point bound, int lo, int hi) {
        	super(bound);
            this.lo = lo;
            this.hi = hi;
        }
        
        public String toString() {
        	return String.format("Subtask(%d-%d)", lo, hi);
        }

		@Override
		public void run() {
            int l = lo;
            int h = hi;
            if (h - l > threshold) {
            	// Divvy up l-h into chunks smaller than threshold:
            	final int g = threshold;
                do {
                    int rh = h;
                    h = (l + h) >>> 1;
                    Subtask t = new Subtask(bound(), h, rh);
                    t.schedule();
                } while (h - l > g);
                
            }
            
            // Run what's left:
            IndexedInterval.this.run(parentEnd, l, h);            	
		}		
	}

}