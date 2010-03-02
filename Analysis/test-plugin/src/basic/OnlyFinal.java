package basic;

import java.util.List;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.Intervals;
import ch.ethz.intervals.guard.Guard;
import ch.ethz.intervals.quals.Creator;
import ch.ethz.intervals.quals.DefinesGhost;
import ch.ethz.intervals.quals.GuardedBy;
import ch.ethz.intervals.quals.Requires;

class OnlyFinal {

    void test(Interval x, @Creator("readableBy x") Data data) { // ERROR Variable "x" was not declared.
    }
    
}