package ch.ethz.intervals.impl;

import pcollections.PSet;
import ch.ethz.intervals.InlineInterval;
import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.RethrownException;
import ch.ethz.intervals.Task;


public class InlineIntervalImpl
extends IntervalImpl 
implements InlineInterval
{
	
	private PSet<Throwable> capturedErrors = null;
	
	InlineIntervalImpl(IntervalImpl parent, Task task) 
	{
		super(parent, task);
		
		// Note: don't do anything here, as task.attachedTo()
		// has been called in super();
	}
	
	@Override
	protected PSet<Throwable> catchErrors(PSet<Throwable> errors) 
	{
		capturedErrors = super.catchErrors(errors);
		return null;
	}

	@Override
	public void execute() 
	{
		Current current = Current.get();
		
		if(current.inter != parent)
			throw new IntervalException.NotExecutedFromParent(current.inter, this);
		
		if(isUnscheduled(current)) {
			current.schedule(this);
			end.join();
			current.updateMostRecent(end);
			
			if(capturedErrors != null)
				throw new RethrownException(capturedErrors);
		} else {
			throw new IntervalException.AlreadyExecuted(this);
		}
	}
	
	@Override
	void didSchedule(boolean explicit, boolean parentEndedNormally) 
	{
		super.didSchedule(explicit, parentEndedNormally);
		
		// Inline intervals cannot be implicitly scheduled.
		// If the parent failed to execute us, it is an
		// error in the parent, so add to their exception
		// list.  This will also cancel async subintervals.
		if(!explicit) {
			if(parentEndedNormally) {
				parent.addVertExceptionSync(
						new IntervalException.InlineIntervalNeverExecuted(this)
				);
			}
			
			// We have to cancel ourselves because we are not in the
			// parent's list of async children, and so would not
			// otherwise be notified of the parent having cancelled
			// its children.
			cancel(start);
		}
	}

	@Override
	public boolean isSynchronous() 
	{
		return true;
	}

}
