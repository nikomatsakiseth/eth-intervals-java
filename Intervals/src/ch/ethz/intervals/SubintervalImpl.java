package ch.ethz.intervals;

final class SubintervalImpl<R> extends Interval {

	final Interval superInterval; // Warning: may be null!
	SubintervalTask<R> task;
	R result;
	
	SubintervalImpl(Point bound, Interval superInterval, SubintervalTask<R> task) {
		super(bound);
		this.superInterval = superInterval;
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

	@Override
	public boolean isReadable() {
		return super.isReadable() || (
				superInterval != null && superInterval.isReadable());
	}

	@Override
	public boolean isWritable() {
		return super.isWritable() || (
				superInterval != null && superInterval.isWritable());
	}
	
	@Override
	public boolean holdsLock(Lock l) {
		return super.holdsLock(l) || (
				superInterval != null && superInterval.holdsLock(l));
	}
	
}
