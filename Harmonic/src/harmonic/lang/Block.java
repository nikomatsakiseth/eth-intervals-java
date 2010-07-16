package harmonic.lang;

import ch.ethz.intervals.Interval;
import harmonic.jcompat.*;

@Ghosts(@Ghost(name = "Parent", bound = Interval.class))
public interface Block<R, A> {
    
    @Requires("method inlineSubOf this.(harmonic.lang.Block#Parent)")
    R value(A argument);
    
}