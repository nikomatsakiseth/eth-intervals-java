package ch.ethz.intervals;

final class StartPoint extends Point {

	public StartPoint(String name, Line line, Point bound, int waitCount,
			Interval interval) {
		super(name, line, bound, waitCount, interval);
	}

	@Override
	boolean isStartPoint() {
		return true;
	}
	
}
