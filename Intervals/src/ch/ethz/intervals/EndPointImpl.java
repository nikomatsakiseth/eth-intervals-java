package ch.ethz.intervals;


class EndPointImpl extends PointImpl {
	
	/** Only used for the root end point. */
	EndPointImpl(int waitCount) {
		super(waitCount);
		flags = 0;
	}

	EndPointImpl(PointImpl bound, int waitCount, int flags) {
		super(bound, waitCount);
		this.flags = flags;
	}
	
	@Override
	public String toString() {
		return String.format("End(%x)", System.identityHashCode(this));
	}
	
}
