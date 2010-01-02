import checkers.util.test.*;
import java.util.*;

public class DeepOverrideInterface {

    public static interface I {
        @Odd String interfaceMethod();
    }

    public static abstract class A {
        public abstract @Odd String abstractMethod();
    }

    public static abstract class B extends A implements I {

    }

    public static class C extends B {
        public String interfaceMethod() {
            return "";
        }
        public @Odd String abstractMethod() {
            return null;
        }
    }

}
