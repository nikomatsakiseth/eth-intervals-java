package harmonic.lang;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation tagged to methods and fields that
 * may legally be invoked or accessed by the compiler during
 * compilation.  
 *
 * <p>Currently, it only makes sense to place this on
 * Java static members, but eventually it will be usable
 * on Harmonic objects as well... once I decide how to handle
 * static fields and methods.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD, ElementType.METHOD})
public @interface StaticCheck {
}