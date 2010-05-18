package ch.ethz.intervals.task;

import java.util.Set;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Task;

public abstract class AbstractTask implements Task {
	
	private final String name;
	
	public AbstractTask() {
		name = null;
	}
	
	public AbstractTask(String name) {
		this.name = name;
	}
	
	@Override
	public void attachedTo(Interval inter) {
	}

	@Override
	public String getName() {
		if(name != null)
			return name;
		else
			return toString();
	}
	
	@Override
	public String toString() {
		if(name != null)
			return name;
		else
			return super.toString();
	}

	@Override
	public Set<? extends Throwable> catchErrors(Set<Throwable> errors) {
		return errors;
	}

}
