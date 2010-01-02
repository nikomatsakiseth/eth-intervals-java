import checkers.interning.quals.*;

import java.util.*;

public class Generics {

    void testGenerics() {

        Map<String, @Interned String> map = null;
        map = new HashMap<String, @Interned String>();

        String a = new String("foo");
        @Interned String b = "bar";

        String notInterned;
        @Interned String interned;

        map.put(a, b); // valid
        map.put(b, a); // error

        notInterned = map.get(a); // valid
        interned = map.get(b);    // valid

        Collection<@Interned String> internedSet;
        Collection<String> notInternedSet;

        notInternedSet = map.keySet(); // valid
        internedSet = map.keySet();    // error

        notInternedSet = map.values(); // error
        internedSet = map.values();    // valid

        HashMap<@Interned String,Vector<@Interned Integer>> all_nums = new HashMap<@Interned String,Vector<@Interned Integer>>();
        Vector<@Interned Integer> v = all_nums.get("Hello");

    }

    // The cells aren't interned, but their contents are
    class CellOfImm<T extends @Interned Object> {
        T value;
        boolean equals(CellOfImm<T> other) {
            return value == other.value; // valid
        }
    }

    List<@Interned String> istrings = new ArrayList<@Interned String>();
    List<String> strings = new ArrayList<String>();
    @Interned String istring = "interned";
    String string = new String("uninterned");

    void testGenerics2() {
        istrings.add(istring);
        istrings.add(string);   // invalid
        strings.add(istring);
        strings.add(string);
        istring = istrings.get(0);
        string = istrings.get(0);
        istring = strings.get(0); // invalid
        string = strings.get(0);
    }

    void testCollections() {
        Collection<String> strings = Collections.unmodifiableCollection(new ArrayList<String>());

        Collection<@Interned String> istrings = Collections.unmodifiableCollection(new ArrayList<@Interned String>()); // valid
    }

    class MyList extends ArrayList<@Interned String> {
        // Correct return value is Iterator<@Interned String>
        public Iterator<String> iterator() { return null; }
    }

    // from VarInfoAux
    static class VIA {
        private static VIA theDefault = new VIA();
        private Map<@Interned String, @Interned String> map;

        void testMap() {
            Map<@Interned String,@Interned String> mymap;
            mymap = theDefault.map;
            mymap = new HashMap<@Interned String,@Interned String>(theDefault.map);
        }
    }

    // type inference
    <T> T id(T m, Object t) { return m; }
    void useID() {
        String o = id("m", null);
    }

    // raw types again
    void testRawTypes() {
        ArrayList lst = null;
        Collections.sort(lst);
    }
}
