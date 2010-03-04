package java.lang;

import ch.ethz.intervals.quals.BaseRequirements;
import ch.ethz.intervals.quals.Creator;

@Creator  
public class Object {
    
    public Object() {}

    @BaseRequirements(instanceMethod="Creator readableBy method")
    public String toString() { return ""; }
    
}