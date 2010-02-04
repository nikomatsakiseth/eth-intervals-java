package basic;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.quals.Requires;
import ch.ethz.intervals.quals.Subinterval;
import ch.ethz.intervals.quals.Happens;
import ch.ethz.intervals.quals.DefinesGhost;
import ch.ethz.intervals.quals.Creator;
import ch.ethz.intervals.quals.GuardedBy;

@DefinesGhost(type="ch.ethz.intervals.Interval")
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
	@Requires(subinterval=@Subinterval(of="interA"))
	void interA1() {} // ERROR (intervals.illegal.path.attr)

    // ----------------------------------------------------------------------
    @GuardedBy("ParseReqsCreator")
    Interval interB;

    // Error occurs for same as reason as interA1()
    @Requires(subinterval=@Subinterval(of="interB"))
    void interB1() {} // ERROR (intervals.illegal.path.attr)

	@Requires(
	    subinterval=@Subinterval(of="interB"),
	    happens=@Happens(before="ParseReqsCreator", after="method")
	)
	void interB2() {}

}
