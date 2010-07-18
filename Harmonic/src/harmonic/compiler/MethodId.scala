package harmonic.compiler

/** Uniquely identifies a particular method,
  * even in the face of overloading etc. */
case class MethodId(
    className: Name.Class,          // Note: NOT significant for equality!
    methodName: Name.Method,
    patterns: List[Pattern.Anon]
) {
    assert(patterns.length == methodName.parts.length)
    
    def is(methodId: MethodId) = (this == methodId)
    
    override def equals(obj: Any): Boolean = {
        obj match {
            case obj: MethodId => 
                (obj eq this) || (obj.methodName.is(methodName) && (obj.patterns == patterns))
            case _ => 
                false
        }
    }
    
    override def toString = {
        "(%s.%s)".format(
            className,
            methodName.parts.zip(patterns).map({
                case (n, p @ Pattern.AnonTuple(_)) => n.toString + p.toString
                case (n, p @ Pattern.AnonVar(Type.Tuple(_))) => n.toString + p.toString
                case (n, p) => "%s(%s)".format(n, p)
            }).mkString(" ")
        )
    }
}

object MethodId {
    
    val noParams = Pattern.AnonTuple(Nil)
    
    val GetParent = MethodId(
        Name.RoIntervalClass,
        Name.Method(List("getParent")),
        List(noParams)
    )
    
    val GetStart = MethodId(
        Name.RoIntervalClass,
        Name.Method(List("getStart")),
        List(noParams)
    )

    val GetEnd = MethodId(
        Name.RoIntervalClass,
        Name.Method(List("getEnd")),
        List(noParams)
    )
    
}