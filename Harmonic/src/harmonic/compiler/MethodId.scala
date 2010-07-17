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
        "(%s.%s%s)".format(
            className,
            methodName.javaName,
            msig
        )
    }
}

object MethodId {
    
    val noParams = Pattern.AnonTuple(Nil)
    
    val GetParent = MethodId(
        Name.RoIntervalClass,
        Name.Method(List("getParent")),
        MethodSignature(Type.RoInterval, List(noParams))
    )
    
    val GetStart = MethodId(
        Name.RoIntervalClass,
        Name.Method(List("getStart")),
        MethodSignature(Type.RoPoint, List(noParams))
    )

    val GetEnd = MethodId(
        Name.RoIntervalClass,
        Name.Method(List("getEnd")),
        MethodSignature(Type.RoPoint, List(noParams))
    )
    
}