package ch.ethz.intervals;

public interface Task<R> {
	
	public R run(Interval<R> current);

}
