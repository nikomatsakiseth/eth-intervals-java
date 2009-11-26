package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.POOL;

import java.util.concurrent.atomic.AtomicInteger;

import ch.ethz.intervals.ThreadPool.WorkItem;
import ch.ethz.intervals.ThreadPool.Worker;

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
 * @param count the number of times to invoke {@code task}
 * @param task the task to invoke
 */
public abstract class IndexedTask extends AbstractTask {
	private final AtomicInteger balance = new AtomicInteger(1);
	private final int count;
	private final int threshold;
	
	private AsyncPoint whenDone;

	protected IndexedTask(int count) {
		this.count = count;
		
		// By default, about 16 times as many tasks as threads.
        int p = POOL.numWorkers;
        this.threshold = (p > 1) ? (1 + count / (p << 4)) : count;		
	}
	
	abstract public void run(Point parentEnd, int fromIndex, int toIndex);

	@Override
	public final void run(Point currentEnd) {
		whenDone = Intervals.asyncPoint(currentEnd, 1);
		
    	Worker worker = POOL.currentWorker();
    	Subtask mt = new Subtask(0, count);
		mt.exec(worker);
	}

	final class Subtask extends WorkItem {
        final int lo;
        final int hi;
        
        Subtask(int lo, int hi) {
            this.lo = lo;
            this.hi = hi;
        }
        
        public String toString() {
        	return String.format("MapBase(%d-%d)", lo, hi);
        }

        @Override
        public void exec(Worker worker) {
            int l = lo;
            int h = hi;
            if (h - l > threshold)
                internalCompute(worker, l, h);
            else
            	run(whenDone, l, h);
            
            int b = balance.decrementAndGet(); 
        	if(Debug.ENABLED)
        		Debug.mapComplete(IndexedTask.this, this, b);
            if(b == 0)
            	whenDone.trigger(1);
        }

        final void internalCompute(Worker worker, int l, int h) {
        	// Divvy up l-h into chunks smaller than threshold:
        	final int g = threshold;
            do {
                int rh = h;
                h = (l + h) >>> 1;
                Subtask r = new Subtask(h, rh);
                int b = balance.getAndIncrement();
                if(Debug.ENABLED)
                	Debug.mapFork(IndexedTask.this, this, r, b+1);
                worker.enqueue(r);                
            } while (h - l > g);
            
            // Run what's left:
            run(whenDone, l, h);
        }
    }
}