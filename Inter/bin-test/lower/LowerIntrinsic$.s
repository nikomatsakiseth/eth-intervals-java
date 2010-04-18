// class version 49.0 (49)
// access flags 1
public class lower/LowerIntrinsic$ implements lower/LowerIntrinsic  {


  // access flags 1
  public method(Llower/LowerIntrinsic;)Ljava/lang/Integer;
    LDC 3
    INVOKESTATIC java/lang/Integer.valueOf (I)Ljava/lang/Integer;
    ASTORE 4
    ALOAD 4
    ASTORE 6
    LDC 4
    INVOKESTATIC java/lang/Integer.valueOf (I)Ljava/lang/Integer;
    ASTORE 5
    ALOAD 5
    ASTORE 2
    ALOAD 6
    ALOAD 2
    INVOKESTATIC inter/compiler/IntrinsicMathGen.plus (Ljava/lang/Integer;Ljava/lang/Integer;)Ljava/lang/Integer;
    ASTORE 3
    ALOAD 3
    ARETURN
}
