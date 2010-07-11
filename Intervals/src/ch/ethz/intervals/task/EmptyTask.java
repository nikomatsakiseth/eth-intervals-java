package ch.ethz.intervals.task;

import ch.ethz.intervals.Interval;

public class EmptyTask extends AbstractTask {
	
	public EmptyTask(String name) {
		super(name);
	}
	
	@Override
	public void run(Interval current) {
	}

}
