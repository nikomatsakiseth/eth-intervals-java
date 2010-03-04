package basic;

import ch.ethz.intervals.*;
import ch.ethz.intervals.quals.*;

public class StaticStuff {
    
    // For now I don't worry about accessing aDataField.  
    // In reality, of course, they'd have to be protected
    // by a lock, or else by the root interval.
    
    @GuardedBy("ch.ethz.intervals.guard.RacyGuard#racy")
    public static @Creator("hbNow") Data staticCompletedData;

    @GuardedBy("ch.ethz.intervals.guard.RacyGuard#racy")
    public static Data staticIncompleteData;

    public void tryToWriteIncompleteDataToCompleted(
        @Creator("writableBy method") Data incompleteData
    ) {
        staticCompletedData = incompleteData; // ERROR Variable "incompleteData" has type "*" which is not a subtype of "*".
    }

    public void tryToWriteIncompleteDataToIncomplete(
        @Creator("writableBy method") Data incompleteData
    ) {
        staticIncompleteData = incompleteData;
    }

    public void tryToWriteCompletedDataToCompleted(
        @Creator("hbNow") Data completedData
    ) {
        staticCompletedData = completedData;
    }

    public void tryToWriteCompletedDataToIncomplete(
        @Creator("hbNow") Data completedData
    ) {
        staticIncompleteData = completedData;
    }

    public int accessCompletedData() {
        return staticCompletedData.integer;
    }

    public int accessIncompleteData() {
        Data data = staticIncompleteData;
        int i = data.integer; // ERROR Guard "data.(ch.ethz.intervals.quals.Creator)" is not readable.
        return i; // ERROR Variable "i" was not declared.
    }
	
}
