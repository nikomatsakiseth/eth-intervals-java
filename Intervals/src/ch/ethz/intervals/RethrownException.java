package ch.ethz.intervals;

import java.util.Set;

public class RethrownException extends IntervalException {	
	private static final long serialVersionUID = 6241387639170113110L;
	final Set<Throwable> allErrors;
	
	public RethrownException(Set<Throwable> t) {
		super(t.iterator().next());
		allErrors = t;
	}
	
}
