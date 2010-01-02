package java.util;
import checkers.nullness.quals.*;
@checkers.quals.DefaultQualifier("checkers.nullness.quals.NonNull")

// This permits null element when using a custom comparator which allows null
public class TreeMap<K extends @Nullable Object, V extends @Nullable Object> extends java.util.AbstractMap<K, V> implements java.util.NavigableMap<K, V>, java.lang.Cloneable, java.io.Serializable {
  private static final long serialVersionUID = 0;
  public TreeMap() { throw new RuntimeException("skeleton method"); }
  public TreeMap(java.util.Comparator<? super K> a1) { throw new RuntimeException("skeleton method"); }
  public TreeMap(java.util.Map<? extends K, ? extends V> a1) { throw new RuntimeException("skeleton method"); }
  public TreeMap(java.util.SortedMap<K, ? extends V> a1) { throw new RuntimeException("skeleton method"); }
  public int size() { throw new RuntimeException("skeleton method"); }
  public boolean containsKey(@Nullable java.lang.Object a1) { throw new RuntimeException("skeleton method"); }
  public boolean containsValue(@Nullable java.lang.Object a1) { throw new RuntimeException("skeleton method"); }
  public @Nullable V get(@Nullable java.lang.Object a1) { throw new RuntimeException("skeleton method"); }
  public java.util.Comparator<? super K> comparator() { throw new RuntimeException("skeleton method"); }
  public @Nullable K firstKey() { throw new RuntimeException("skeleton method"); }
  public @Nullable K lastKey() { throw new RuntimeException("skeleton method"); }
  public void putAll(java.util.Map<? extends K, ? extends V> a1) { throw new RuntimeException("skeleton method"); }
  public @Nullable V put(K a1, V a2) { throw new RuntimeException("skeleton method"); }
  public @Nullable V remove(@Nullable java.lang.Object a1) { throw new RuntimeException("skeleton method"); }
  public void clear() { throw new RuntimeException("skeleton method"); }
  public @Nullable java.util.Map.Entry<K, V> firstEntry() { throw new RuntimeException("skeleton method"); }
  public @Nullable java.util.Map.Entry<K, V> lastEntry() { throw new RuntimeException("skeleton method"); }
  public @Nullable java.util.Map.Entry<K, V> pollFirstEntry() { throw new RuntimeException("skeleton method"); }
  public @Nullable java.util.Map.Entry<K, V> pollLastEntry() { throw new RuntimeException("skeleton method"); }
  public @Nullable java.util.Map.Entry<K, V> lowerEntry(K a1) { throw new RuntimeException("skeleton method"); }
  public @Nullable K lowerKey(K a1) { throw new RuntimeException("skeleton method"); }
  public @Nullable java.util.Map.Entry<K, V> floorEntry(K a1) { throw new RuntimeException("skeleton method"); }
  public @Nullable K floorKey(K a1) { throw new RuntimeException("skeleton method"); }
  public @Nullable java.util.Map.Entry<K, V> ceilingEntry(K a1) { throw new RuntimeException("skeleton method"); }
  public @Nullable K ceilingKey(K a1) { throw new RuntimeException("skeleton method"); }
  public @Nullable java.util.Map.Entry<K, V> higherEntry(K a1) { throw new RuntimeException("skeleton method"); }
  public @Nullable K higherKey(K a1) { throw new RuntimeException("skeleton method"); }
  public java.util.Set<K> keySet() { throw new RuntimeException("skeleton method"); }
  public java.util.NavigableSet<K> navigableKeySet() { throw new RuntimeException("skeleton method"); }
  public java.util.NavigableSet<K> descendingKeySet() { throw new RuntimeException("skeleton method"); }
  public java.util.Collection<V> values() { throw new RuntimeException("skeleton method"); }
  public java.util.Set<java.util.Map.Entry<K, V>> entrySet() { throw new RuntimeException("skeleton method"); }
  public java.util.NavigableMap<K, V> descendingMap() { throw new RuntimeException("skeleton method"); }
  public java.util.NavigableMap<K, V> subMap(K a1, boolean a2, K a3, boolean a4) { throw new RuntimeException("skeleton method"); }
  public java.util.NavigableMap<K, V> headMap(K a1, boolean a2) { throw new RuntimeException("skeleton method"); }
  public java.util.NavigableMap<K, V> tailMap(K a1, boolean a2) { throw new RuntimeException("skeleton method"); }
  public java.util.SortedMap<K, V> subMap(K a1, K a2) { throw new RuntimeException("skeleton method"); }
  public java.util.SortedMap<K, V> headMap(K a1) { throw new RuntimeException("skeleton method"); }
  public java.util.SortedMap<K, V> tailMap(K a1) { throw new RuntimeException("skeleton method"); }
  public Object clone() { throw new RuntimeException("skeleton method"); }
}
