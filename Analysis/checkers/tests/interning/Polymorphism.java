import checkers.interning.quals.*;
import java.util.*;
import java.lang.ref.WeakReference;

public class Polymorphism {
    // Test parameter
    public @PolyInterned String identity(@PolyInterned String s) {
        return s;
    }

    void testParam() {
        String notInterned = new String("not interned");
        @Interned String interned = "interned";

        interned = identity(interned);
        interned = identity(notInterned); // invalid
    }

    // test as receiver
    @PolyInterned Polymorphism getSelf() @PolyInterned {
        return this;
    }

    void testReceiver() {
        Polymorphism notInterned = new Polymorphism();
        @Interned Polymorphism interned = null;

        interned = interned.getSelf();
        interned = notInterned.getSelf(); // invalid
    }

    // Test assinging interned to PolyInterned
    public @PolyInterned String always(@PolyInterned String s) {
        if (s.equals("n"))
            return "m";
        else
            return new String("m"); // invalid
    }

    public static @PolyInterned Object[] id(@PolyInterned Object[] a) {
        return a;
    }

    public static void idTest (@Interned Object @Interned [] seq) {
        @Interned Object[] copy_uninterned = id(seq);
    }

    private static Map<List<@Interned String @Interned []>,WeakReference<@Interned String @Interned []>> internedStringSequenceAndIndices;
    private static List<@Interned String @Interned []> sai;
    private static WeakReference<@Interned String @Interned []> wr;

    public static void testArrayInGeneric() {
        internedStringSequenceAndIndices.put (sai, wr);
    }

    // check for a crash when using raw types
    void processMap(Map<String, String> map) { }
    void testRaw() {
        Map m = null;
        processMap(m);
    }

    // test anonymous classes
    private void testAnonymous() {
        new Object() {
            public boolean equals(Object o) { return true; }
        }.equals(null);

        Date d = new Date() { };
    }
}
