package ch.ethz.intervals;


class EndPointImpl extends PointImpl {
	
	/** If this flag is set, then uncaught exceptions for this 
	 *  interval are NOT propagated to its parent. */
	static final int FLAG_MASK_EXC = 1;
	
	protected int flags;
	
	/** Only used for the root end point. */
	EndPointImpl(int waitCount) {
		super(waitCount);
		flags = 0;
	}

	EndPointImpl(PointImpl bound, int waitCount, int flags) {
		super(bound, waitCount);
		this.flags = flags;
	}
	
	void addFlagBeforeScheduling(int flag) {
		this.flags = this.flags | flag;
	}

	void removeFlagBeforeScheduling(int flag) {
		this.flags = this.flags & (~flag);
	}

	@Override
	protected void occur() {
		EdgeList notifyEdges;
		synchronized(this) {
			waitCount = OCCURRED;
			notifyEdges = outEdges;
			notifyAll();
		}
		
		ExecutionLog.logArrive(this);

		if((flags & FLAG_MASK_EXC) == 0 && throwable != null)
			bound.setThrowableFromChild(throwable);
		
		for(EdgeList outEdge = notifyEdges; outEdge != null; outEdge = outEdge.next)
			outEdge.toPoint.arrive(1);
		bound.arrive(1);
	}
	
	@Override
	public String toString() {
		return String.format("End(%x)", System.identityHashCode(this));
	}
	
}
