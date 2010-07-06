package harmonic.compiler

/** Uniquely identifies a particular method,
  * even in the face of overloading etc. */
case class MethodId(
    className: Name.Class,
    methodName: Name.Method,
    msig: MethodSignature[Pattern.Anon]
) {
    def is(methodId: MethodId) = (this == methodId)
    
    override def toString = {
        "(%s.%s)%s".format(
            className,
            methodName.javaName,
            msig
        )
    }
}

object MethodId {
    
    val GetStart = MethodId(
        Name.IntervalClass,
        Name.Method(List("getStart")),
        MethodSignature(Type.Point, List())
    )

    val GetEnd = MethodId(
        Name.IntervalClass,
        Name.Method(List("getEnd")),
        MethodSignature(Type.Point, List())
    )
    
}