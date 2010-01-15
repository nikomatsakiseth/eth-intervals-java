package ch.ethz.intervals;

import ch.ethz.intervals.quals.Creator;
import ch.ethz.intervals.quals.GuardedBy;
import ch.ethz.intervals.quals.WrittenDuring;

/**
 * A guard is an object that defines when a field may be safely read or written.
 * There are three subtypes of {@link Guard}, each with different properties: 
 * <ol>
 * 
 * <li>{@link Lock}: Fields guarded by a {@link Lock} may only be read or written 
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
 * you can use the methods {@link Guard#checkReadable()} or 
 * {@link Guard#checkWritable()} as a kind of escape clause.  Simply insert
 * an assertion like {@code assert g.checkWritable()} and the compiler
 * will respect it.  
 */
@Creator("this.constructor")
public interface Guard {
	
	/** If {@code g} is not readable by the current method, throws an appropriate
	 *  exception.  Otherwise returns true.  The return value is a convenience 
	 *  method so that it can be used in an assert like {@code assert g.checkReadable();}. */
	public boolean checkReadable();
		
	/** If {@code g} is not writable by the current method, throws an appropriate
	 *  exception.  Otherwise returns true.  The return value is a convenience 
	 *  method so that it can be used in an assert like {@code assert g.checkWritable();}. */
	public boolean checkWritable(); 
	
}
