package ch.ethz.intervals;

public interface SubintervalTask<R> {	
	public R run(Interval subinterval);
}
