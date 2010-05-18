package harmonic.runtime;

import ch.ethz.intervals.*;

/** Miscellaneous helper methods referenced in ByteCode.scala */
public class Helper {
    public static void addHb(AsyncInterval from, AsyncInterval to) {
        Intervals.addHb(from, to);
    }

    public static void addHb(Point from, AsyncInterval to) {
        Intervals.addHb(from, to);
    }

    public static void addHb(AsyncInterval from, Point to) {
        Intervals.addHb(from, to);
    }

    public static void addHb(Point from, Point to) {
        Intervals.addHb(from, to);
    }
}