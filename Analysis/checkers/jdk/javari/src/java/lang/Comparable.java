package java.lang;
import checkers.javari.quals.*;
import java.util.*;

public interface Comparable<T> {
    public int compareTo(T o) @ReadOnly;
}
