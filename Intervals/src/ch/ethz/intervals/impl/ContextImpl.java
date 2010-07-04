package ch.ethz.intervals.impl;

import static ch.ethz.intervals.util.ChunkList.NORMAL;
import ch.ethz.intervals.Context;
import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.ScopedVar;
import ch.ethz.intervals.Lock;
import ch.ethz.intervals.Task;
import ch.ethz.intervals.guard.Guard;

public class ContextImpl implements Context {
	
	public static final Context intervals = new ContextImpl();
	
	/** Shared thread pool that executes tasks. */
	public static final ThreadPool POOL = new ThreadPool();

	private ContextImpl() {
		
	}
	
	@Override
	public Lock lock() {
		return null;
	}
	
	public boolean checkReadable(Guard guard) 
	{
		Current current = Current.get();
		if(current.inter == null)
			throw new IntervalException.NotInRootInterval(); // later we could allow this maybe
		RuntimeException err = guard.checkReadable(current.mr, current.inter);
		if(err != null) throw err;
		return true;
	}
	
	public boolean checkWritable(Guard guard) 
	{
		Current current = Current.get();
		if(current.inter == null)
			throw new IntervalException.NotInRootInterval(); // later we could allow this maybe
		RuntimeException err = guard.checkWritable(current.mr, current.inter);
		if(err != null) throw err;
		return true;
	}

	public InlineIntervalImpl unexecutedInline(final Task task)
	{		
		Current current = Current.get();
		return new InlineIntervalImpl(current.inter, task);
	}

	@Override
	public int getNumWorkers() {
		return POOL.numWorkers;
	}

	@Override
	public <T> ScopedVar<T> scopedVar(T defaultValue) {
		return new ScopedVarImpl<T>(defaultValue);
	}

	
}
