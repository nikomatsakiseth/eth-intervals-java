package ch.ethz.intervals.quals;

import java.lang.annotation.ElementType;
import java.lang.annotation.Target;

@Target({
	ElementType.FIELD,
	ElementType.PARAMETER,
	ElementType.METHOD
})
public @interface Is {
	public String value();
}
