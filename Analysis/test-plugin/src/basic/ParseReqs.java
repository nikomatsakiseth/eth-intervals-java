package basic;

import ch.ethz.intervals.Interval;
import ch.ethz.intervals.quals.DefinesGhost;
import ch.ethz.intervals.quals.GuardedBy;
import ch.ethz.intervals.quals.Happens;
import ch.ethz.intervals.quals.Requires;
import ch.ethz.intervals.quals.Subinterval;

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
	@Requires(subinterval=@Subinterval(of="interA"))
	void interA1() {} // ERROR Path "this.interA" must be immutable to be used here.

    // ----------------------------------------------------------------------
    @GuardedBy("ParseReqsCreator")
    Interval interB;

    // Error occurs for same as reason as interA1()
    @Requires(subinterval=@Subinterval(of="interB"))
    void interB1() {} // ERROR Path "this.interB" must be immutable to be used here.

	@Requires(
	    happens=@Happens(before="ParseReqsCreator", after="method"),
	    subinterval=@Subinterval(of="interB")
	)
	void interB2() {}

}
