package ch.ethz.intervals.quals;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/** Specifies the path to the guard for a particular field. */
@Target({ElementType.FIELD})
@Retention(RetentionPolicy.RUNTIME)
public @interface GuardedBy {
        public String value();
}
