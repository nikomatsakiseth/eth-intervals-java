package harmonic.jcompat;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target({})
public @interface Ghost {
    /** Name of the ghost member */
    public String name();
    
    /** Ghost can only be bound to instances of this class */
    public Class<?> bound();
}