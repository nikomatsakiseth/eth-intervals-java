package java.io;

import checkers.nullness.quals.*;

@checkers.quals.DefaultQualifier("checkers.nullness.quals.NonNull")

public class LineNumberReader extends BufferedReader {
  public LineNumberReader(java.io.Reader a1) { super(a1); throw new RuntimeException("skeleton method"); }
  public LineNumberReader(java.io.Reader a1, int a2) { super(a1); throw new RuntimeException("skeleton method"); }
  public void setLineNumber(int a1) { throw new RuntimeException("skeleton method"); }
  public int getLineNumber() { throw new RuntimeException("skeleton method"); }
  public int read() throws java.io.IOException { throw new RuntimeException("skeleton method"); }
  public int read(char[] a1, int a2, int a3) throws java.io.IOException { throw new RuntimeException("skeleton method"); }
  public @Nullable java.lang.String readLine() throws java.io.IOException { throw new RuntimeException("skeleton method"); }
  public long skip(long a1) throws java.io.IOException { throw new RuntimeException("skeleton method"); }
  public void mark(int a1) throws java.io.IOException { throw new RuntimeException("skeleton method"); }
  public void reset() throws java.io.IOException { throw new RuntimeException("skeleton method"); }
}
