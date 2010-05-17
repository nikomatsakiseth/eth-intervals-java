package ch.ethz.intervals.mirror;

import ch.ethz.intervals.guard.Guard;

public interface Context {
	
	/** 
	 * An estimate of the number of worker threads, or put 
	 * another way, amount of available parallelism. */
	public int getNumWorkers();
	
	public Lock lock();
	
	public boolean checkReadable(Guard guard);
	
	public boolean checkWritable(Guard guard);
		
	public void inline(Task task);
	
}
