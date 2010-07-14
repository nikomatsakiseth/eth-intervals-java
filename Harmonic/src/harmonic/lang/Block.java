package harmonic.lang;

import ch.ethz.intervals.Interval;

@Ghosts(@Ghost(name = "Parent", cls = Interval.class))
public interface Block<R, A> {
    
    @Requires("method inlineSubOf this.(harmonic.lang.Block#Parent)")
    R value(A argument);
    
}