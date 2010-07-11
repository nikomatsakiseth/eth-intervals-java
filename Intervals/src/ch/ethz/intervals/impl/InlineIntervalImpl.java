package ch.ethz.intervals.impl;

import static ch.ethz.intervals.util.ChunkList.NORMAL;
import pcollections.PSet;
import ch.ethz.intervals.InlineInterval;
import ch.ethz.intervals.IntervalException;
import ch.ethz.intervals.RethrownException;
import ch.ethz.intervals.Task;


public final class InlineIntervalImpl
extends IntervalImpl 
implements InlineInterval
{
	
	private PSet<Throwable> capturedErrors = null;
	
	InlineIntervalImpl(IntervalImpl parent, Task task) 
	{
		super(parent, task);
			
		boolean success = false;
		try {
			// Note: this must be done last, once
			// the interval is otherwise fully constructed.
			task.attachedTo(this);
			success = true;
		} finally {
			if(!success) // is there a more elegant way to run code only on error?
				cancel(false);
		}
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
		
		if(isUnscheduled(current)) {

			// Add an edge from end of previous inline interval to start of this:
			if(current.mr != null && current.mr != current.start())
				current.mr.addEdgeAfterOccurredWithoutException(start, NORMAL);
			
			current.schedule(this);
			end.join();
			current.updateMostRecent(end);
			
			if(capturedErrors != null)
				throw new RethrownException(capturedErrors);
		} else {
			throw new IntervalException.AlreadyScheduled(this);
		}
	}
	
	@Override
	public void cancel(boolean unconditionally)
	{
		Current current = Current.get();
		
		if(isUnscheduled(current)) {
			cancel(start);
			current.schedule(this);
			assert(capturedErrors == null);
		} else if (unconditionally) {
			throw new IntervalException.AlreadyScheduled(this);
		}
	}
	
	@Override
	void didSchedule(boolean explicit) 
	{
		super.didSchedule(explicit);
		
		// Inline intervals cannot be implicitly scheduled.
		// If the parent failed to execute us, it is an
		// error in the parent, so add to their exception
		// list.  This will also cancel async subintervals.
		if(!explicit) {
			parent.addVertExceptionSync(
					new IntervalException.InlineIntervalNeverExecuted(this)
			);
			
			// We have to cancel ourselves because we are not in the
			// parent's list of async children, and so would not
			// otherwise be notified of the parent having cancelled
			// its children.
			cancel(start);
		}
	}

	@Override
	public boolean isInline() 
	{
		return true;
	}

}
