package ch.ethz.intervals.task;

import ch.ethz.intervals.Interval;

public class EmptyTask extends AbstractTask {
	
	private final String name;
	
	public EmptyTask(String name) {
		this.name = name;
	}
	
	@Override
	public String toString() {
		return name;
	}

	@Override
	public void run(Interval current) {
	}

}
