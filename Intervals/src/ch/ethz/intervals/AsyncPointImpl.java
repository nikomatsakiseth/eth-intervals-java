package ch.ethz.intervals;

class AsyncPointImpl extends PointImpl implements AsyncPoint {
	
	private int triggerCount;
	
	AsyncPointImpl(Current unscheduled, PointImpl bound, int triggerCount) {
		super(unscheduled, bound, 1);
		this.triggerCount = triggerCount;
	}

	@Override
	public void trigger(int cnt) {
		boolean arrived = false;
		synchronized(this) {
			if(this.triggerCount > 0) {
				int newCount = this.triggerCount - cnt;
				this.triggerCount = Math.max(0, newCount);
				arrived = (newCount <= 0); 
			}
		}		
		if(arrived)
			arrive(1);
	}

	@Override
	public String toString() {
		return String.format("Async(%x)", System.identityHashCode(this));
	}
	
}
