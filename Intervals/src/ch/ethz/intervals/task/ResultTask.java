package ch.ethz.intervals.task;

import ch.ethz.intervals.mirror.Interval;


public abstract class ResultTask<R> extends AbstractTask {
	
	private R result;
	
	public ResultTask() {
		super();
	}

	public ResultTask(String name) {
		super(name);
	}

	@Override
	public final void run(Interval current) throws Exception {
		result = compute(current);
	}
	
	public R getResult() {
		return result;
	}
	
	protected abstract R compute(Interval current) throws Exception;
	
}
