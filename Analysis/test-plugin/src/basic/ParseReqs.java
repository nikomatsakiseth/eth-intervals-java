package basic;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.quals.DefinesGhost;
import ch.ethz.intervals.quals.GuardedBy;
import ch.ethz.intervals.quals.Requires;

@DefinesGhost(ofClass=Interval.class)
@interface ParseReqsCreator {
    public String value() default "";
}

@ParseReqsCreator
public class ParseReqs {
	
    // ----------------------------------------------------------------------
	// Guarded by @Creator by default:
	Interval interA;
	
	// Error occurs because 'interA' is not stable at the
	// the time subinter1 runs:
	@Requires("method suspends interA")
	void interA1() {} // ERROR Path "this.(basic.ParseReqs.interA)" must be immutable to be used here.

    // ----------------------------------------------------------------------
    @GuardedBy("ParseReqsCreator")
    Interval interB;

    // Error occurs for same as reason as interA1()
	@Requires("method suspends interB")
    void interB1() {} // ERROR Path "this.(basic.ParseReqs.interB)" must be immutable to be used here.

	@Requires({
		"ParseReqsCreator hb method",
		"method suspends interB"
	})
	void interB2() {}

}
