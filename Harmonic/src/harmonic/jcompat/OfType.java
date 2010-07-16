package harmonic.jcompat;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation applied to a method defined in another annotation 
 * which indicates that its value is a harmonic path.  
 *
 * @see harmonic.lang.Mutable#value()
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD})
public @interface OfType {
    public AnnType value();
}