package ch.ethz.intervals;

public class AlreadyScheduledException extends IntervalException {
	AlreadyScheduledException() {
		super("Interval already scheduled!");
	}
}
