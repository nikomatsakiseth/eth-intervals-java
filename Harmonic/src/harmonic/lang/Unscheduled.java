package harmonic.lang;

import ch.ethz.intervals.Interval;

/** When applied to an interval class member,
  * indicates that the interval should not be
  * scheduled automatically once the class is
  * constructed.  You are then responsible for
  * invoking {@link Interval#schedule()} yourself,
  * or else the interval will be automatically 
  * scheduled when the current interval / inline
  * interval has completed.
  */
public @interface Unscheduled {
  
}