package java.io;

import checkers.nullness.quals.*;

@checkers.quals.DefaultQualifier("checkers.nullness.quals.NonNull")

public abstract interface FileFilter{
  public abstract boolean accept(java.io.File a1);
}
