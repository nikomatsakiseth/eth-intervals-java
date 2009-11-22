package ch.ethz.intervals;

public interface IndexedTask {

	/** Process indices i where start <= i < stop. */
	void run(Interval<Void> parent, int start, int stop);
	
}
