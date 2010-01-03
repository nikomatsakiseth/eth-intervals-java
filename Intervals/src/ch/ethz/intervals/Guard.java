package ch.ethz.intervals;

import ch.ethz.intervals.params.creator;
import ch.ethz.intervals.quals.GuardedBy;
import ch.ethz.intervals.quals.WrittenDuring;

/**
 * A guard is an object that defines when a field may be safely read or written.
 * There are three subtypes of {@link Guard}, each with different properties: 
 * <ol>
 * 
 * <li>{@link Lock}: Fields guarded by a {@link Lock} may only be accessed 
 * during an interval which holds the lock.  
 *  
 * <li>{@link Interval}: Fields guarded by an {@link Interval} {@code inter} may only be
 * written by the {@link Interval#run()} method of {@code inter}, but they
 * may be read by an interval which {@code inter} <em>happens before</em>.\
 * 
 * <li>{@link DynamicGuard}: Fields guarded by a dynamic guard are dynamically monitored
 * for data-race violations.  Every write must <em>happen before</em> all later writes
 * and reads.  
 * 
 * </ol>
 *
 * <p>To indicate the guard for a field, use a {@link GuardedBy} or {@link WrittenDuring}
 * annotation.  Note that guard annotations are only enforced when using our 
 * {@code Javac} plugin.  All guard types except {@link DynamicGuard} are enforced statically; 
 * violations of {@link DynamicGuard} are enforced dynamically and the required instrumentation
 * is automatically inserted by the plugin.
 * 
 * <p>If the compiler plugin cannot statically prove the safety of your program,
 * you can use the methods {@link Guard#isReadable()} or 
 * {@link Guard#isWritable()} as a kind of escape clause.  Simply insert
 * an assertion like {@code assert g.isWritable()} and the compiler
 * will respect it.  
 */
@creator("this.constructor")
public interface Guard {
	/** True if the current method is permitted to read data protected by {@code this}. */
	public boolean isReadable();
	
	/** True if the current method is permitted to write data protected by {@code this}. */
	public boolean isWritable();
}
