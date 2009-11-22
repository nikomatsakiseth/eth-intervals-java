package ch.ethz.intervals;


class StartPointImpl extends PointImpl {

	public StartPointImpl(PointImpl bound, int waitCount) {
		super(bound, waitCount);
	}
	
	@Override
	public String toString() {
		return String.format("Start(%x)", System.identityHashCode(this));
	}
	
}
