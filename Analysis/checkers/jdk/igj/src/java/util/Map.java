package java.util;
import checkers.igj.quals.*;

@I
public interface Map<K, V> {
  @I
  public interface Entry<K, V> {
    public abstract K getKey() @ReadOnly;
    public abstract V getValue() @ReadOnly;
    public abstract V setValue(V a1) @AssignsFields;
    public abstract boolean equals(@ReadOnly java.lang.Object a1) @ReadOnly;
    public abstract int hashCode() @ReadOnly;
  }
  public abstract int size() @ReadOnly;
  public abstract boolean isEmpty() @ReadOnly;
  public abstract boolean containsKey(@ReadOnly java.lang.Object a1) @ReadOnly;
  public abstract boolean containsValue(@ReadOnly java.lang.Object a1) @ReadOnly;
  public abstract V get(@ReadOnly java.lang.Object a1) @ReadOnly ;
  public abstract V put(K a1, V a2) @Mutable;
  public abstract V remove(@ReadOnly java.lang.Object a1) @Mutable;
  public abstract void putAll(@ReadOnly java.util.Map<? extends K, ? extends V> a1) @Mutable;
  public abstract void clear() @Mutable;
  public abstract @I java.util.Set<K> keySet() @ReadOnly;
  public abstract @I java.util.Collection<V> values() @ReadOnly;
  public abstract @I java.util.Set<@I java.util.Map.Entry<K, V>> entrySet() @ReadOnly;
  public abstract boolean equals(@ReadOnly java.lang.Object a1) @ReadOnly ;
  public abstract int hashCode() @ReadOnly ;
}
