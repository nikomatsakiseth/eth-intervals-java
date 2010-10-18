package ch.ethz.intervals.impl;

import ch.ethz.intervals.Condition;
import ch.ethz.intervals.ConditionUnknown;
import ch.ethz.intervals.Context;
import ch.ethz.intervals.Interval;
import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.Lock;
import ch.ethz.intervals.ScopedVar;
import ch.ethz.intervals.Task;
import ch.ethz.intervals.guard.DynamicGuard;
import ch.ethz.intervals.guard.Guard;
import ch.ethz.intervals.task.AbstractTask;

public class ContextImpl implements Context {
	
	public static final Context intervals = new ContextImpl();
	
	/** Shared thread pool that executes tasks. */
	public static final ThreadPool POOL = new ThreadPool();

	private ContextImpl() {
		
	}
	
	@Override
	public Lock lock(String name) {
		return new LockImpl(name);
	}
	
	private boolean checkCondition(Current current, Condition cond) {
		if(!cond.isTrueFor(current.mr, current.inter)) {
			throw new IntervalException.ConditionDoesNotHold(
					cond, 
					current.mr, 
					current.inter);
		}
		return true;
	}
	
	public boolean checkReadable(Guard guard) 
	{
		Current current = Current.get();
		if(current.inter == null)
			throw new IntervalException.NotInRootInterval(); // later we could allow this maybe
		return checkCondition(
				current, 
				guard.condWritableBy().or(guard.condReadableBy()).or(guard.condFinal()));
	}

	public boolean checkWritable(Guard guard) 
	{
		Current current = Current.get();
		if(current.inter == null)
			throw new IntervalException.NotInRootInterval(); // later we could allow this maybe
		return checkCondition(current, guard.condWritableBy());
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

	@Override
	public void join(final Interval toJoin) {
		if(!toJoin.getEnd().didOccur()) {
			Intervals.inline(new AbstractTask("join:"+toJoin.toString()) {
				@Override public void attachedTo(Interval inter) {
					super.attachedTo(inter);
					toJoin.getEnd().addHb(inter.getStart());
				}

				@Override public void run(Interval current) throws Exception {
				}
			});
		}
	}

	@Override
	public void makeWritable(DynamicGuard guard) {
		Current current = Current.get();
		guard.makeWritableBy(current.mr, current.inter);
	}

	@Override
	public void makeReadable(DynamicGuard guard) {
		Current current = Current.get();
		guard.makeReadableBy(current.mr, current.inter);
	}

	@Override
	public void makeFinal(DynamicGuard guard) {
		Current current = Current.get();
		guard.makeFinal(current.mr, current.inter);
	}
	
}
