package java.util;
import checkers.nullness.quals.*;
@checkers.quals.DefaultQualifier("checkers.nullness.quals.NonNull")

public final class Currency implements java.io.Serializable {
    private static final long serialVersionUID = 0L;
  protected Currency() {}
  public static java.util.Currency getInstance(java.lang.String a1) { throw new RuntimeException("skeleton method"); }
  public static java.util.Currency getInstance(java.util.Locale a1) { throw new RuntimeException("skeleton method"); }
  public java.lang.String getCurrencyCode() { throw new RuntimeException("skeleton method"); }
  public java.lang.String getSymbol() { throw new RuntimeException("skeleton method"); }
  public java.lang.String getSymbol(java.util.Locale a1) { throw new RuntimeException("skeleton method"); }
  public int getDefaultFractionDigits() { throw new RuntimeException("skeleton method"); }
  public java.lang.String toString() { throw new RuntimeException("skeleton method"); }
}
