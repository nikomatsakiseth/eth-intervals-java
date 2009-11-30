package ch.ethz.intervals;

public interface Task {
	
	/** When a new interval is created that will execute this task,
	 *  it invokes this method to give the task a chance to add
	 *  any necessary dependencies. */
	public void addDependencies(Interval inter);
	
	/** Executed by an interval associated with this task when its
	 *  start point occurs. */
	public void run(Point currentEnd);

}
