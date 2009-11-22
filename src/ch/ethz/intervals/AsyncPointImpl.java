package ch.ethz.intervals;

class AsyncPointImpl extends PointImpl implements AsyncPoint {
	
	AsyncPointImpl(PointImpl bound, int waitCount) {
		super(bound, waitCount);
	}

	@Override
	protected void occur() {
		defaultOccur();
		bound.arrive(1);
	}
	
	@Override
	public void arrive(int cnt) {
		super.arrive(cnt);
	}

	@Override
	public String toString() {
		return String.format("Async(%x)", System.identityHashCode(this));
	}
	
}
