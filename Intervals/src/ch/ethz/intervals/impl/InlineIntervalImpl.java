package ch.ethz.intervals.impl;

import pcollections.PSet;
import ch.ethz.intervals.InlineInterval;
import ch.ethz.intervals.RethrownException;
import ch.ethz.intervals.Task;


public class InlineIntervalImpl
extends IntervalImpl 
implements InlineInterval
{
	
	private PSet<Throwable> capturedErrors = null;
	
	InlineIntervalImpl(IntervalImpl parent, Task task) {
		super(parent, task);
		
		// Note: don't do anything here, as task.attachedTo()
		// has been called in super();
	}
	
	@Override
	protected PSet<Throwable> catchErrors(PSet<Throwable> errors) {
		capturedErrors = super.catchErrors(errors);
		return null;
	}

	@Override
	public void execute() 
	{
		Current current = Current.get();
		current.schedule(this);
		end.join();
		current.updateMostRecent(end);
		
		if(capturedErrors != null)
			throw new RethrownException(capturedErrors);
	}

	@Override
	public boolean isSynchronous() {
		return true;
	}

}
