package harmonic.compiler

/** Uniquely identifies a particular method,
  * even in the face of overloading etc. */
case class MethodId(
    className: Name.Class,
    methodName: Name.Method,
    msig: MethodSignature[Pattern.Anon]
) {
    override def toString = {
        "%s.%s%s".format(
            className,
            methodName.javaName,
            msig
        )
    }
}