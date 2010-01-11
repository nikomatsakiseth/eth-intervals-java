package ch.ethz.intervals;

final class SubintervalImpl<R> extends Interval {

	SubintervalTask<R> task;
	R result;
	
	SubintervalImpl(Interval superInterval, Point start, Point end, SubintervalTask<R> task) {
		super(superInterval, start, end);
		assert superInterval.line == start.line;
		this.task = task;
	}
	
	@Override
	public String toString() {
		return task.toString();
	}

	@Override
	protected void run() {
		result = task.run(this);
	}
	
}
