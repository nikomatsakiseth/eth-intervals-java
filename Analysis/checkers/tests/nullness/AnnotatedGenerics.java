import checkers.nullness.quals.*;

class AnnotatedGenerics {
    public static void testNullableTypeVariable() {
        class Test<T extends @Nullable Object> {
            T f;
            @Nullable T get() { return f; }
        }
        Test<Iterable<String>> l = new Test<Iterable<String>>();
        for (String s : l.get());
    }

    public static void testNonNullTypeVariable() {
        class Test<T extends @Nullable Object> {
            @NonNull T get() { throw new RuntimeException(); }
        }
        Test<@Nullable Iterable<String>> l = new Test<@Nullable Iterable<String>>();
        for (String s : l.get());
        Test<Iterable<String>> n = new Test<Iterable<String>>();
        for (String s : n.get());
    }

    static class MyClass<T> implements MyIterator<@Nullable T> {
        public boolean hasNext() { return true; }
        public @Nullable T next() { return null; }
        public void remove() { }
        static void test() {
            MyClass<String> c = new MyClass<String>();
            String c1 = c.next();
            @Nullable String c2 = c.next();
            @NonNull String c3 = c.next();
        }
    }

    public static final class MyComprator<T extends MyComparable<T>> {
        public void compare(T a1, T a2) {
          a1.compareTo(a2);
        }
      }

      class MyComparable<T> {
        public int compareTo(@NonNull T a1) { return 0; }
      }

      <T> T test(java.util.List<? super Iterable<?>> l) {
          test(new java.util.ArrayList<Object>());
          throw new Error();
      }

      public interface MyIterator<E extends @Nullable Object> {
          boolean hasNext();
          E next();
          void remove();
      }

}
