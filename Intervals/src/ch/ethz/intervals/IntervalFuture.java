package ch.ethz.intervals;

import java.util.concurrent.Future;

public interface IntervalFuture<R> 
extends Future<R>
{
	/** Returns the end point of the interval. */
	Point end();
	
	/** 
	 * Accesses the result of this interval.  This is only 
	 * permitted when (a) the current interval comes after
	 * endPoint() or (b) endPoint is bounded by the current
	 * interval. 
	 * 
	 * @throws RethrownException if the interval for which
	 * this is a result failed to complete normally. */
	R result();
}
