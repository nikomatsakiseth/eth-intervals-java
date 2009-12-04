package ch.ethz.intervals;

public abstract class IntervalException extends RuntimeException {
	private static final long serialVersionUID = -7494012490281334869L;

	public IntervalException() {
	}

	public IntervalException(String arg0) {
		super(arg0);
	}

	public IntervalException(Throwable arg0) {
		super(arg0);
	}

	public IntervalException(String arg0, Throwable arg1) {
		super(arg0, arg1);
	}

}
