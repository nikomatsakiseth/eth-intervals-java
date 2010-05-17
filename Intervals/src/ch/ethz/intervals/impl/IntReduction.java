package ch.ethz.intervals.impl;


//******************************************************
// Note: IntReduction and LongReduction are automatically
// generated.  Edit the version for DOUBLES only and run
// makereductions.sh!
//******************************************************

/**
 * Reductions allow efficient, race-free parallel addition
 * and subtraction. 
 */
// @JPartParams("@V @W=@V")
public class IntReduction {
	
	private static final int PAD = 4;
	
	// @In("@V")
	private int value;
	
	// @In("@W")
	private final int[] values;
	
	private volatile IntReduction next;
	
	public IntReduction(int initialValue) {
		value = initialValue;
		int workers = ContextImpl.POOL.numWorkers;
		values = new int[workers * PAD];
	}
	
	private IntReduction(int initialValue, int arraySize) {
		value = initialValue;
		values = new int[arraySize];
	}

	
	/**
	 * Technically, it's possible for a fork-join pool to grow more
	 * workers over time.  Therefore, we actually keep a linked list
	 * of reductions.  I would just grow the array, but I'd need a read-write
	 * lock then, and I don't want that.
	 */
	private void addToNext(int amnt, int index) {
		int length = values.length;
		IntReduction next = this.next;
		if(next == null) {
			synchronized(this) {
				next = this.next;
				if(next == null) {
					int parSize = ContextImpl.POOL.numWorkers * PAD - length;
					int newSize = Math.max(parSize, index - length + PAD);
					this.next = next = new IntReduction(0, newSize);
				}
			}
		}
		
		next.add(amnt, index - length);
	}
	
	private void add(int amnt, int index) {
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
	public void add(int amnt) {
		int index = ContextImpl.POOL.currentWorker().id * PAD;
		add(amnt, index);
	}
	
	/**
	 * Invoked from a worker interval to subtract some amount from the value
	 * that is returned on the next call to {@link #reduce()}. 
	 * 
	 * @param amnt amount to be subtracted on next reduce
	 */
	// @JPartMethod(effects="AtomicWr(@W)")
	public void subtract(int amnt) {
		int index = ContextImpl.POOL.currentWorker().id * PAD;
		add(-amnt, index);
	}
	
	/** Computes total difference since last invocation, and resets
	 *  accumulators. */
	// @JPartMethod(effects="Access(@W)")
	private int diff() {
		int diff = 0;
		for (int i = 0; i < values.length; i += PAD) {
			diff += values[i];
			values[i] = 0;
		}
		
		IntReduction next = this.next;
		if(next != null)
			diff += next.diff();
		
		return diff;
	}

	/**
	 * Accumulates all changes from worker threads and returns
	 * the new value.  
	 * 
	 * <p>A data-race occurs if this method overlaps
	 * with calls to {@link #add(int)} or {@link #subtract(int)}.
	 * 
	 * @return the newly accumulated value    
	 */
	// @JPartMethod(effects="Access(@V | @W)")
	public int reduce() {
		value += diff();
		return value;
	}

	/**
	 * Sets the current value to {@code v} and resets any
	 * interal counters.  
	 * 
	 * <p>A data-race occurs if this method overlaps
	 * with calls to {@link #add(int)} or {@link #subtract(int)}.
	 * 
	 * @param v new value
	 */
	// @JPartMethod(effects="Wr(@V | @W)")
	public void resetAll(int v) {
		value = v;
		resetAccumulators();
	}
	
	/**
	 * Discards any accumulation that has occurred since the last
	 * {@link #reduce()}.
	 * 
	 * <p>A data-race occurs if this method overlaps
	 * with calls to {@link #add(int)}, {@link #subtract(int)},
	 * {@link #reduce()} etc.
	 */
	// @JPartMethod(effects="Wr(@W)")
	public void resetAccumulators() {
		diff();
	}
	
	/** 
	 * Read current value without taking into account any 
	 * calls to {@link #add(int)} or {@link #subtract(int)}
	 * that have occurred since the last {@link #reduce()}
	 * or {@link #resetAll(int)}.
	 * 
	 * <p>A data-race occurs if this method overlaps
	 * with calls to {@link #reduce()}, {@link #resetAll(int)},
	 * or {@link #setValue(int)}.
	 */
	// @JPartMethod(effects="Rd(@V)")
	public int value() {
		return value;
	}
	
	/** 
	 * Read current value without taking into account any 
	 * calls to {@link #add(int)} or {@link #subtract(int)}
	 * that have occurred since the last {@link #reduce()}
	 * or {@link #resetAll(int)}.
	 * 
	 * <p>A data-race occurs if this method overlaps
	 * with calls to {@link #reduce()}, {@link #resetAll(int)},
	 * or {@link #value()}.
	 */
	// @JPartMethod(effects="Wr(@V)")
	public void setValue(int v) {
		value = v;
	}
}
