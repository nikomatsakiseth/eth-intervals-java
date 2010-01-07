package ch.ethz.intervals;

final class SubintervalImpl<R> extends Interval {

	SubintervalTask<R> task;
	R result;
	
	SubintervalImpl(Line line, Point start, Point end, SubintervalTask<R> task) {
		super(line, start, end);
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
