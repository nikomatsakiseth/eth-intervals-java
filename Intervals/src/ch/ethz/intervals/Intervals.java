package ch.ethz.intervals;


public class Intervals {
	
	static class NamedTask implements Task {
		public final String name;
		public NamedTask(String name) {
			super();
			this.name = name;
		}
		public void run(Point currentEnd) {
		}
		public String toString() {
			return name;
		}		
	}
	
	/** Convenient task that does nothing. */
	public static final Task emptyTask = new NamedTask("emptyTask");
	static final Task readTask = new NamedTask("readTask");

	static final PointImpl ROOT_END = new PointImpl(1); // never occurs
	static final PointImpl ROOT_START = new PointImpl(ROOT_END, PointImpl.OCCURRED); // always occurred
	static final IntervalImpl ROOT_INTERVAL = new IntervalImpl(null, ROOT_START, ROOT_END);
	
	static final ThreadPool POOL = new ThreadPool();

	/** Currently executing interval in each thread, if any. */
	static ThreadLocal<IntervalImpl> currentInterval = 
		new ThreadLocal<IntervalImpl>() {
			@Override
			protected IntervalImpl initialValue() {
				return ROOT_INTERVAL;
			}
		};
	
	/** 
	 * Returns {@code i.start()} unless {@code i} is null, 
	 * in which case just returns null. */
	public static Point start(Interval i) {
		if(i == null)
			return null;
		return i.start();
	}
	
	/** 
	 * Returns {@code i.end()} unless {@code i} is null, 
	 * in which case just returns null. */
	public static Point end(Interval i) {
		if(i == null)
			return null;
		return i.end();
	}
	
	/** 
	 * Creates and returns a new unscheduled interval with no bound.
	 * You can add additional dependencies by invoking methods on the
	 * {@code UnscheduledInterval} object.  To obtain the new interval,
	 * it must be scheduled using {@link UnscheduledInterval#schedule(Task)}.
	 * 
	 * Once scheduled, the resulting interval will execute in 
	 * parallel with the caller.  To create a blocking interval, use
	 * {@link #blockingInterval(Task)}.
	 * 
	 * @see #intervalWithBound(Point)
	 * @see #blockingInterval(Task)
	 */
	public static UnscheduledInterval interval() {
		return intervalWithBound(ROOT_END);
	}
	
	/** 
	 * Creates and returns a new unscheduled interval with a bound
	 * {@code bnd}.  A bound is effectively a dependency {@code endBefore(bnd)},
	 * but has additional ramifications when doing dynamic race detection.
	 * 
	 * You can add additional dependencies by invoking methods on the
	 * {@code UnscheduledInterval} object.  To obtain the new interval,
	 * it must be scheduled using {@link UnscheduledInterval#schedule(Task)}.
	 *  
	 * Once scheduled, the resulting interval will execute in 
	 * parallel with the caller.  To create a blocking interval, use
	 * {@link #blockingInterval(Task)}.
	 * 
	 * @see #interval()
	 * @see #intervalDuring(Interval)
	 * @see #blockingInterval(Task)
	 */
	public static UnscheduledInterval intervalWithBound(Point bnd) {
		return new UnscheduledIntervalImpl((PointImpl) bnd);
	}
	
	/** 
	 * Creates and returns a new unscheduled interval with a bound
	 * {@code interval.end()} and which always starts after
	 * {@code interval.start()}.
	 * 
	 * You can add additional dependencies by invoking methods on the
	 * {@code UnscheduledInterval} object.  To obtain the new interval,
	 * it must be scheduled using {@link UnscheduledInterval#schedule(Task)}.
	 *  
	 * Once scheduled, the resulting interval will execute in 
	 * parallel with the caller.  To create a blocking interval, use
	 * {@link #blockingInterval(Task)}.
	 * 
	 * @see #interval()
	 * @see #intervalWithBound(Point) 
	 * @see #blockingInterval(Task)
	 */
	public static UnscheduledInterval intervalDuring(Interval interval) {
		return intervalWithBound(interval.end()).startAfter(interval.start());
	}
	
	/**
	 * @see AsyncPoint
	 */
	public static AsyncPoint asyncPoint(Point bound, int cnt) {
		checkCurrentIntervalEndHbOrSame(bound);
		PointImpl boundImpl = (PointImpl) bound;
		boundImpl.addWaitCount();
		return new AsyncPointImpl(boundImpl, cnt);
	}
	
	/**
	 * If set to false, disables all safety checks against
	 * cycles or race conditions.  
	 */
	public static final boolean SAFETY_CHECKS = true;	

	static void checkEdge(Point from, Point to) {
		if (SAFETY_CHECKS && !from.hb(to))
			throw new NoEdgeException(from, to);
	}
	
	static void checkEdgeOrSame(Point from, Point to) {
		if(from != to)
			checkEdge(from, to);
	}

	static void checkCurrentIntervalEndHbOrSame(Point to) {
		if(SAFETY_CHECKS) {
			checkEdgeOrSame(currentInterval.get().end, to);
		}
	}

	/** Waits for {@code ep} to complete and returns its result.
	 *  Resets the currentInterval afterwards. */
	static void join(IntervalImpl current, PointImpl pnt) {
		try {
			if(Debug.ENABLED)
				Debug.join(current, pnt);
			pnt.join();
		} finally {
			// Helping out other tasks can disrupt this value, so restore it
			currentInterval.set(current); 
		}
	}
	
	/**
	 * Waits for {@code ep} to occur and returns its result, possibly rethrowing any 
	 * exception.  There must be a path from {@code ep} to the end point
	 * of the current interval.
	 * 
	 * <b>Note:</b> Exceptions that occur in {@code task} are 
	 * wrapped in {@link RethrownException} and rethrown immediately,
	 * but may also propagate upwards depending on whether 
	 * {@code ep} was configured to mask errors with 
	 * {@link UnscheduledInterval#setMaskExceptions(boolean)}.
	 */
	public static <R> R blockOn(IntervalFuture<R> result) {
		blockOn(result.end());
		IntervalFutureImpl<R> resultImpl = (IntervalFutureImpl<R>) result;
		return resultImpl.accessResult();
	}
	
	public static void blockOn(Point pnt) {
		IntervalImpl current = currentInterval.get();
		checkEdge(pnt, current.end);
		join(current, (PointImpl) pnt);
	}
	
	/**
	 * Creates a new interval which executes during the current interval.
	 * This interval will execute {@code task}.  This function does not
	 * return until the new interval has completed.
	 * 
	 * <b>Note:</b> Exceptions that occur in {@code task} are 
	 * wrapped in {@link RethrownException} and rethrown immediately.
	 * Exceptions never propagate to the current interval.
	 */
	public static void blockingInterval(Task task) 
	{
		IntervalImpl current = currentInterval.get();
		PointImpl end = (PointImpl) intervalWithBound(current.end)
			.setMaskExceptions(true)
			.schedule(task)
			.end();
		join(current, end);
	}
	
	/** 
	 * Returns the root interval which represents the entire
	 * computation.  The point {@code root.start()} has already
	 * occurred once program execution begins, and {@code root.end()}
	 * will not occur until program execution completes. */
	public static Interval root() {
		return ROOT_INTERVAL;
	}

}
