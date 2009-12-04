/**
 * 
 */
package ch.ethz.intervals;

public class NamedTask extends AbstractTask {
	public final String name;
	public NamedTask(String name) {
		super();
		this.name = name;
	}
	public void run(Point currentEnd) {
	}
	public String toString() {
		return name;
	}		
}