package java.util;
import checkers.nullness.quals.*;
@checkers.quals.DefaultQualifier("checkers.nullness.quals.NonNull")

public final class PropertyPermission extends java.security.BasicPermission {
    private static final long serialVersionUID = 0L;
  public PropertyPermission(java.lang.String a1, @Nullable java.lang.String a2) { super(a1); throw new RuntimeException("skeleton method"); }
  public boolean implies(java.security.Permission a1) { throw new RuntimeException("skeleton method"); }
  public boolean equals(@Nullable java.lang.Object a1) { throw new RuntimeException("skeleton method"); }
  public int hashCode() { throw new RuntimeException("skeleton method"); }
  public java.lang.String getActions() { throw new RuntimeException("skeleton method"); }
  public java.security.PermissionCollection newPermissionCollection() { throw new RuntimeException("skeleton method"); }
}
