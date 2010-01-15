import ch.ethz.intervals.Dependency;
import ch.ethz.intervals.Interval;

public class TaskInterval extends Interval {
	
	final Task task;

	public TaskInterval(Dependency dep, Task task) {
		super(dep, task.toString());
		this.task = task;
	}

	@Override
	protected void run() {
		task.run();
	}
	
}
