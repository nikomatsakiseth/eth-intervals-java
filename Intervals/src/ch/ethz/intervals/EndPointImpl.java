package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.checkEdge;

class EndPointImpl<V> extends PointImpl implements EndPoint<V> {
	
	/** If this flag is set, then uncaught exceptions for this 
	 *  interval are NOT propagated to its parent. */
	static final int FLAG_MASK_EXC = 1;
	
	protected V result;
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
	public V result() {
		checkEdge(this, Intervals.currentInterval.get().start());
		return accessResult();		
	}
	
	protected V accessResult() {
		checkThrowable();
		return result;
	}

	synchronized void setResult(V result) {
		this.result = result;
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
