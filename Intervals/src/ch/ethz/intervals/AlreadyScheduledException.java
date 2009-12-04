package ch.ethz.intervals;

/** Internal error indicating that an interval was scheduled twice. */
public class AlreadyScheduledException extends IntervalException {
	private static final long serialVersionUID = 3969920588351084252L;

	AlreadyScheduledException() {
		super("Interval already scheduled!");
	}
}
