package ch.ethz.intervals;

/** Indicates that a data race was detected on a {@link DynamicGuard} */
public class DataRaceException extends RuntimeException {

	enum Role { READ, WRITE, LOCK };
	
	public final Guard guard;
	public final Role beforeRole;
	public final Point before;
	public final Role afterRole;
	public final Point after;
	
	public DataRaceException(
			Guard guard,
			Role beforeRole,
			Point before,
			Role afterRole,
			Point after)
	{
		this.guard = guard;
		this.beforeRole = beforeRole;
		this.before = before;
		this.afterRole = afterRole;
		this.after = after;
	}
	
}
