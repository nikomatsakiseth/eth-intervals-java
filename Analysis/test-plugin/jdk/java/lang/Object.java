package java.lang;

import ch.ethz.intervals.quals.Creator;
import ch.ethz.intervals.quals.Requires;

@Creator 
public class Object {
    
    public Object() {}

    @Requires("Creator readableBy method")
    public String toString() { return ""; }
    
}