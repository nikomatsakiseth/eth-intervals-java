package ch.ethz.intervals;

/** Indicates that a data race was detected on a {@link DynamicGuard} */
public class DataRaceException extends RuntimeException {
	private static final long serialVersionUID = -4846309821441326261L;

	enum Role { READ, WRITE, LOCK };

	public final DynamicGuard dg;      /** Guard on which the race occurred. */
	public final Role acc;             /** Kind of access which failed. */
	public final Interval interloper;  /** Interval performing failed access. */
	public final Interval owner;       /** Current owner. */
	
	public DataRaceException(DynamicGuard g, Role acc, Interval interloper, Interval owner) 
	{
		this.dg = g;
		this.acc = acc;
		this.interloper = interloper;
		this.owner = owner;
	}
	
}
