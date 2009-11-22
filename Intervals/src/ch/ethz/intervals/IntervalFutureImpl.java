package ch.ethz.intervals;

import static ch.ethz.intervals.Intervals.checkEdge;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

class IntervalFutureImpl<R> 
implements IntervalFuture<R> {
	
	private final PointImpl endPoint;
	private R result;
	
	public IntervalFutureImpl(PointImpl endPoint) {
		this.endPoint = endPoint;
	}

	@Override
	public PointImpl end() {
		return endPoint;
	}

	synchronized void setResult(R result) {
		this.result = result;
	}
	
	protected R accessResult() {
		endPoint.checkThrowable();
		return result;
	}
	
	@Override
	public R result() {
		checkEdge(endPoint, Intervals.currentInterval.get().start());
		return accessResult();
	}
	
	@Override
	public boolean cancel(boolean mayInterruptIfRunning) {
		return false;
	}

	@Override
	public R get() {
		// XXX
		return result();
	}

	@Override
	public R get(long timeout, TimeUnit unit) throws InterruptedException,
			ExecutionException, TimeoutException {
		// XXX
		return result();
	}

	@Override
	public boolean isCancelled() {
		return false;
	}

	@Override
	public boolean isDone() {
		return false;
	}


}
