/**
 * 
 */
package erco.intervals.tsp;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.VoidInlineTask;

public final class RunSolver extends VoidInlineTask {
	private final Config config;

	public RunSolver(Config config) {
		this.config = config;
	}

	@Override public void run(Interval subinterval) {
		new /*@ch.ethz.intervals.Parent("subinterval")*/ TspSolver(subinterval, config);
	}
}