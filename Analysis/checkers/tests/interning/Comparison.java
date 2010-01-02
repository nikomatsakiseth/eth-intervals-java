import checkers.interning.quals.*;

import java.util.*;

public class Comparison {

    void testInterned() {

        @Interned String a = "foo";
        @Interned String b = "bar";

        if (a == b) {
            System.out.println("yes");
        } else {
            System.out.println("no");
        }

        if (a != b) {
            System.out.println("no");
        } else {
            System.out.println("yes");
        }
    }

    void testNotInterned() {

        String c = new String("foo");
        String d = new String("bar");

        if (c == d) {
            System.out.println("yes");
        } else {
            System.out.println("no");
        }

        if (c != d) {
            System.out.println("no");
        } else {
            System.out.println("yes");
        }
    }
}
