package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.POOL;

//******************************************************
// Note: IntReduction and LongReduction are automatically
// generated.  Edit the version for DOUBLES only and run
// makereductions.sh!
//******************************************************

// @JPartParams("@V @W=@V")
public class LongReduction {
	
	private static final int PAD = 4;
	
	// @In("@V")
	private long value;
	
	// @In("@W")
	private final long[] values;
	
	private volatile LongReduction next;
	
	public LongReduction(long initialValue) {
		value = initialValue;
		int workers = POOL.numWorkers;
		values = new long[workers * PAD];
	}
	
	private LongReduction(long initialValue, int arraySize) {
		value = initialValue;
		values = new long[arraySize];
	}

	
	/**
	 * Technically, it's possible for a fork-join pool to grow more
	 * workers over time.  Therefore, we actually keep a linked list
	 * of reductions.  I would just grow the array, but I'd need a read-write
	 * lock then, and I don't want that.
	 */
	private void addToNext(long amnt, int index) {
		int length = values.length;
		LongReduction next = this.next;
		if(next == null) {
			synchronized(this) {
				next = this.next;
				if(next == null) {
					int parSize = POOL.numWorkers * PAD - length;
					int newSize = Math.max(parSize, index - length + PAD);
					this.next = next = new LongReduction(0, newSize);
				}
			}
		}
		
		next.add(amnt, index - length);
	}
	
	private void add(long amnt, int index) {
		if (index >= values.length) {
			addToNext(amnt, index);
		} else {		
			values[index] += amnt;
		}
	}
	
	/**
	 * Invoked from a worker interval to add some amount to the value
	 * that is returned on the next call to {@link #reduce()}.
	 * 
	 * @param amnt amount to be added on next reduce
	 */
	// @JPartMethod(effects="AtomicWr(@W)")
	public void add(long amnt) {
		int index = POOL.currentWorker().id * PAD;
		add(amnt, index);
	}
	
	/**
	 * Invoked from a worker interval to subtract some amount from the value
	 * that is returned on the next call to {@link #reduce()}. 
	 * 
	 * @param amnt amount to be subtracted on next reduce
	 */
	// @JPartMethod(effects="AtomicWr(@W)")
	public void subtract(long amnt) {
		int index = POOL.currentWorker().id * PAD;
		add(-amnt, index);
	}
	
	/** Computes total difference since last invocation, and resets
	 *  accumulators. */
	// @JPartMethod(effects="Access(@W)")
	private long diff() {
		long diff = 0;
		for (int i = 0; i < values.length; i += PAD) {
			diff += values[i];
			values[i] = 0;
		}
		
		LongReduction next = this.next;
		if(next != null)
			diff += next.diff();
		
		return diff;
	}

	/**
	 * Accumulates all changes from worker threads and returns
	 * the new value.  
	 * 
	 * <p>A data-race occurs if this method overlaps
	 * with calls to {@link #add(long)} or {@link #subtract(long)}.
	 * 
	 * @return the newly accumulated value    
	 */
	// @JPartMethod(effects="Access(@V | @W)")
	public long reduce() {
		value += diff();
		return value;
	}

	/**
	 * Sets the current value to {@code v} and resets any
	 * interal counters.  
	 * 
	 * <p>A data-race occurs if this method overlaps
	 * with calls to {@link #add(long)} or {@link #subtract(long)}.
	 * 
	 * @param v new value
	 */
	// @JPartMethod(effects="Wr(@V | @W)")
	public void resetAll(long v) {
		value = v;
		resetAccumulators();
	}
	
	/**
	 * Discards any accumulation that has occurred since the last
	 * {@link #reduce()}.
	 * 
	 * <p>A data-race occurs if this method overlaps
	 * with calls to {@link #add(long)}, {@link #subtract(long)},
	 * {@link #reduce()} etc.
	 */
	// @JPartMethod(effects="Wr(@W)")
	public void resetAccumulators() {
		diff();
	}
	
	/** 
	 * Read current value without taking into account any 
	 * calls to {@link #add(long)} or {@link #subtract(long)}
	 * that have occurred since the last {@link #reduce()}
	 * or {@link #resetAll(long)}.
	 * 
	 * <p>A data-race occurs if this method overlaps
	 * with calls to {@link #reduce()}, {@link #resetAll(long)},
	 * or {@link #setValue(long)}.
	 */
	// @JPartMethod(effects="Rd(@V)")
	public long value() {
		return value;
	}
	
	/** 
	 * Read current value without taking into account any 
	 * calls to {@link #add(long)} or {@link #subtract(long)}
	 * that have occurred since the last {@link #reduce()}
	 * or {@link #resetAll(long)}.
	 * 
	 * <p>A data-race occurs if this method overlaps
	 * with calls to {@link #reduce()}, {@link #resetAll(long)},
	 * or {@link #value()}.
	 */
	// @JPartMethod(effects="Wr(@V)")
	public void setValue(long v) {
		value = v;
	}
}
