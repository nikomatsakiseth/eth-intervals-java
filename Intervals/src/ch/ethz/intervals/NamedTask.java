/**
 * 
 */
package ch.ethz.intervals;

/**
 * An empty task whose {@link #toString()} function returns the given name.
 * Useful for grouping together related tasks in a way that is easy to see
 * in the intervals visualizer.
 */
public class NamedTask extends AbstractTask {
	public final String name;
	public NamedTask(String name) {
		super();
		this.name = name;
	}
	
	@Override
	public void run(Point currentEnd) {
	}
	
	@Override
	public String toString() {
		return name;
	}		
}