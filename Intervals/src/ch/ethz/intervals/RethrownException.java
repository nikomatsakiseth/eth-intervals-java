package ch.ethz.intervals;

public class RethrownException extends IntervalException {
	
	public RethrownException(Throwable t) {
		super(t);
	}
	
}
