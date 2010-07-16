package harmonic.jcompat;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Java annotations are limited to certain data types.  Harmonic
 * annotations are slightly more general-- in particular, they may
 * include paths (like {@code this.bar}) or facts (like 
 * {@code bar.permitsWr(foo)}).  This enumeration defines the additional
 * types permitted in harmonic annotations.
 *
 * @see harmonic.jcompat.OfType
 */
public enum AnnType {
    PATH, FACT
}