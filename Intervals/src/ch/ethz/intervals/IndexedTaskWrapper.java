/**
 * 
 */
package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.POOL;

import java.util.concurrent.atomic.AtomicInteger;

import ch.ethz.intervals.ThreadPool.WorkItem;
import ch.ethz.intervals.ThreadPool.Worker;

class IndexedTaskWrapper implements Task<Void> {
	private final AtomicInteger balance = new AtomicInteger(1);
	private final int count;
	private final IndexedTask task;
	private final int threshold;
	
	private Interval<Void> parent;
	private AsyncPoint whenDone;

	IndexedTaskWrapper(int count, IndexedTask task) {
		this.count = count;
		this.task = task;
		
		// By default, about 16 times as many tasks as threads.
        int p = POOL.numWorkers;
        this.threshold = (p > 1) ? (1 + count / (p << 4)) : count;		
	}

	@Override
	public String toString() {
		return task.toString();
	}

	@Override
	public Void run(Interval<Void> current) {
		parent = current;
		whenDone = Intervals.asyncPoint(current.end(), 1);
		
    	Worker worker = POOL.currentWorker();
    	Subtask mt = new Subtask(0, count);
		mt.exec(worker);
		return null;
	}

    @SuppressWarnings("serial")
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
            	task.run(parent, l, h);
            
            int b = balance.decrementAndGet(); 
        	if(Debug.ENABLED)
        		Debug.mapComplete(IndexedTaskWrapper.this, this, b);
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
                	Debug.mapFork(IndexedTaskWrapper.this, this, r, b+1);
                worker.enqueue(r);                
            } while (h - l > g);
            
            // Run what's left:
            task.run(parent, l, h);
        }
    }
}