package java.io;

import checkers.nullness.quals.*;

@checkers.quals.DefaultQualifier("checkers.nullness.quals.NonNull")

@Deprecated
public class LineNumberInputStream extends FilterInputStream {
  public LineNumberInputStream(java.io.InputStream a1) { throw new RuntimeException("skeleton method"); }
  public int read() throws java.io.IOException { throw new RuntimeException("skeleton method"); }
  public int read(byte[] a1, int a2, int a3) throws java.io.IOException { throw new RuntimeException("skeleton method"); }
  public long skip(long a1) throws java.io.IOException { throw new RuntimeException("skeleton method"); }
  public void setLineNumber(int a1) { throw new RuntimeException("skeleton method"); }
  public int getLineNumber() { throw new RuntimeException("skeleton method"); }
  public int available() throws java.io.IOException { throw new RuntimeException("skeleton method"); }
  public void mark(int a1) { throw new RuntimeException("skeleton method"); }
  public void reset() throws java.io.IOException { throw new RuntimeException("skeleton method"); }
}
