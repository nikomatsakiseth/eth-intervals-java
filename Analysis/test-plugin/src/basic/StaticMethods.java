package basic;

import ch.ethz.intervals.*;
import ch.ethz.intervals.quals.*;

public class StaticMethods {
    
    // For now I don't worry about accessing aDataField.  
    // In reality, of course, they'd have to be protected
    // by a lock, or else by the root interval.
    
    @GuardedBy("ch.ethz.intervals.guard.RacyGuard#racy")
    public static @Creator("hbNow") Data staticCompletedData;
    
    public @Creator("hbNow") Data completedData;
    
    public static void tryToWriteIncompleteDataToCompleted(
        @Creator("writableBy method") Data incompleteData
    ) {
        staticCompletedData = incompleteData; // ERROR Variable "incompleteData" has type "*" which is not a subtype of "*".
    }

    public static int accessStaticCompletedDataStatically() {
        return staticCompletedData.integer;
    }

    public int invokeStaticMethod() {
        return accessStaticCompletedDataStatically();
    }
    
    public int invokeStaticMethodWithExplicitClassName() {
        return StaticMethods.accessStaticCompletedDataStatically();
    }
    
    public int invokeStaticMethodWithVeryExplicitClassName() {
        return basic.StaticMethods.accessStaticCompletedDataStatically();
    }
    
    public int accessStaticFieldsOfInteger() {
        return Integer.MAX_VALUE;
    }
    
}
