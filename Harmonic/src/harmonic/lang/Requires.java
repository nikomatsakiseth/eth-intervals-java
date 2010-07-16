package harmonic.lang;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import harmonic.jcompat.OfType;
import harmonic.jcompat.AnnType;

@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface Requires {
    @OfType(AnnType.PATH) 
    public String[] value();
}