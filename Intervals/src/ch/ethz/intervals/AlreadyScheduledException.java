package ch.ethz.intervals;

public class AlreadyScheduledException extends IntervalException {
	private static final long serialVersionUID = 3969920588351084252L;

	AlreadyScheduledException() {
		super("Interval already scheduled!");
	}
}
