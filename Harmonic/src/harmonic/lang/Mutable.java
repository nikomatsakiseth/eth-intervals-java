package harmonic.lang;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import harmonic.jcompat.OfType;
import harmonic.jcompat.AnnType;

/**
 * Annotation applies to fields and local variables to 
 * indicate that they are mutable and --- optionally --- the
 * path to their guard.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD})
public @interface Mutable {
    @OfType(AnnType.PATH) 
    public String value();
}