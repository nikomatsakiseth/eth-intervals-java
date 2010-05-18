package harmonic.runtime;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.task.AbstractTask;

/** The classes we generate in Harmonic do not need
  * or use the pointer to the current interval.  Therefore,
  * we use this base class so as to have a run method
  * with no arguments.  It makes life a little easier. */
public abstract class HarmonicTask extends AbstractTask {
    
    public HarmonicTask(String name) {
        super(name);
    }
    
    public abstract void run();
    
    @Override
    public final void run(Interval current) {
        run();
    }
    
}