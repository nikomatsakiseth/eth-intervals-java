package ch.ethz.intervals;

class EndPoint extends Point {
	
	public EndPoint(String name, Line line, Point bound, int waitCount,
			Interval interval) {
		super(name, line, bound, waitCount, interval);
	}

	@Override
	boolean isStartPoint() {
		return false;
	}

}
