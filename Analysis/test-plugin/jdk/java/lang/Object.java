package java.lang;

import ch.ethz.intervals.quals.Creator;
import ch.ethz.intervals.quals.Requires;
import ch.ethz.intervals.quals.Readable;

@Creator 
public class Object {
    
    public Object() {}

    @Requires(readable=@Readable("Creator"))
    public String toString() { return ""; }
    
}