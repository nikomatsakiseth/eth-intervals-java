package ch.ethz.intervals;


public class Intervals {
	
	static class NamedTask implements Task<Void> {
		public final String name;
		public NamedTask(String name) {
			super();
			this.name = name;
		}
		public Void run(Interval<Void> current) {
			return null;
		}
		public String toString() {
			return name;
		}		
	}
	
	/** Convenient task that does nothing. */
	public static final Task<Void> emptyTask = new NamedTask("emptyTask");
	static final Task<Void> readTask = new NamedTask("readTask");

	static final PointImpl ROOT_END = new PointImpl(1); // never occurs
	static final PointImpl ROOT_START = new PointImpl(ROOT_END, PointImpl.OCCURRED); // always occurred
	static final IntervalImpl<Void> ROOT_INTERVAL = new IntervalImpl<Void>(null, ROOT_START, ROOT_END);
	
	static final ThreadPool POOL = new ThreadPool();

	/** Currently executing interval in each thread, if any. */
	static ThreadLocal<IntervalImpl<?>> currentInterval = 
		new ThreadLocal<IntervalImpl<?>>() {
			@Override
			protected IntervalImpl<?> initialValue() {
				return ROOT_INTERVAL;
			}
		};
	
	/** 
	 * Returns {@code i.start()} unless {@code i} is null, 
	 * in which case just returns null. */
	public static Point start(Interval<?> i) {
		if(i == null)
			return null;
		return i.start();
	}
	
	/** 
	 * Returns {@code i.end()} unless {@code i} is null, 
	 * in which case just returns null. */
	public static Point end(Interval<?> i) {
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
	public static UnscheduledInterval intervalDuring(Interval<?> interval) {
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
	static <R> R join(IntervalImpl<?> current, IntervalFutureImpl<R> result) {
		join(current, result.end());
		return result.accessResult();
	}

	/** Waits for {@code ep} to complete and returns its result.
	 *  Resets the currentInterval afterwards. */
	static void join(IntervalImpl<?> current, PointImpl pnt) {
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
	 * A more efficient way of creating {@code count} distinct
	 * intervals and joining them immediately.  {@code task}
	 * will be invoked with every integer from 0 to {@code count-1}.
	 * Creates an outer interval which encompasses all
	 * computations from 0 to {@code count-1}, but may create
	 * any number of subintervals to do the actual computations
	 * in parallel.  The task is always invoked with the outer
	 * interval, which is always an ancestor of the current interval
	 * but may not be the current interval itself.  
	 * 
	 * @param count the number of times to invoke {@code task}
	 * @param task the task to invoke
	 */
	public static void blockingIndexedInterval(
			final int count,
			final IndexedTask task) 
	{
		blockingInterval(new IndexedTaskWrapper(count, task));
	}
	
	/**
	 * Embodies a task which requires an initial "setup" phase that 
	 * creates the interval structure that performs
	 * the actual computation.  The method {@link SetupTask#setup(Interval, Interval)}
	 * is invoked with two intervals: the first is the current,
	 * setup interval.  The second is a sibling which will not start
	 * until the setup interval has completed.  The setup interval can therefore
	 * create new work during the next interval using
	 * {@link #intervalDuring(Interval)}, safe in the knowledge that none
	 * of these tasks will execute until the setup interval is complete.  
	 * 
	 * @param <R> The result of the setup interval (typically {@link Void})
	 * @param setupTask The setup task.
	 * @return the value returned by the setup interval, 
	 * once the mutual bound of the setup interval and its sibling
	 * has finished 
	 */
	public static <R> R blockingSetupInterval(
			final SetupTask<R> setupTask)
	{
		Task<R> outerTask = new SetupTaskWrapper<R>(setupTask);
		return blockingInterval(outerTask);
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
		IntervalImpl<?> current = currentInterval.get();
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
	public static <R> R blockingInterval(Task<R> task) 
	{
		IntervalImpl<?> current = currentInterval.get();
		IntervalFuture<R> result = intervalWithBound(current.end)
			.setMaskExceptions(true)
			.schedule(task)
			.future();
		return join(current, (IntervalFutureImpl<R>)result);
	}
	
	/** 
	 * Returns the root interval which represents the entire
	 * computation.  The point {@code root.start()} has already
	 * occurred once program execution begins, and {@code root.end()}
	 * will not occur until program execution completes. */
	public static Interval<?> root() {
		return ROOT_INTERVAL;
	}

}
