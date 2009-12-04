package ch.ethz.intervals;

import java.util.Collections;
import java.util.Set;

/** When a task or subinterval results in an exception, those exceptions
 *  are propagated along the graph and eventually rethrown.  When
 *  rethrown, they are wrappd in this class.  Because more than one error
 *  may have been accumulated, you may need to refer to the set
 *  {@link #allErrors()}. */
public class RethrownException extends IntervalException {	
	private static final long serialVersionUID = 6241387639170113110L;
	private final Set<Throwable> allErrors;
	
	public RethrownException(Set<Throwable> t) {
		super(t.iterator().next());
		allErrors = Collections.unmodifiableSet(t);
	}
	
	public Set<Throwable> allErrors() {
		return allErrors;
	}
	
}
