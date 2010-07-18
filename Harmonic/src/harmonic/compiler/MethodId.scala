package harmonic.compiler

sealed trait MethodId {
    def className: Name.Class
    def methodName: Name.Method
    def patterns: List[Pattern.Anon]
    
    def is(methodId: MethodId) = (this == methodId)
    
    assert(patterns.length == methodName.parts.length)
    
    protected[this] def argString: String = {
        methodName.parts.zip(patterns).map({
            case (n, p @ Pattern.AnonTuple(_)) => n.toString + p.toString
            case (n, p @ Pattern.AnonVar(Type.Tuple(_))) => n.toString + p.toString
            case (n, p) => "%s(%s)".format(n, p)
        }).mkString(" ")        
    }
}

object MethodId {
    case class Static(
        className: Name.Class,
        methodName: Name.Method,
        patterns: List[Pattern.Anon]
    ) extends MethodId {
        override def toString = "(%s.%s)".format(className, argString)
    }

    case class Virtual(
        className: Name.Class,          // Note: NOT significant for equality!
        methodName: Name.Method,
        patterns: List[Pattern.Anon]
    ) extends MethodId {
        override def equals(obj: Any): Boolean = {
            obj match {
                case obj: MethodId => 
                    (obj eq this) || (obj.methodName.is(methodName) && (obj.patterns == patterns))
                case _ => 
                    false
            }
        }
    
        override def toString = "(%s.%s)".format(className, argString)
    }

    val GetParent = Virtual(
        Name.RoIntervalClass,
        Name.Method(List("getParent")),
        List(Pattern.EmptyAnonTuple)
    )
    
    val GetStart = Virtual(
        Name.RoIntervalClass,
        Name.Method(List("getStart")),
        List(Pattern.EmptyAnonTuple)
    )

    val GetEnd = Virtual(
        Name.RoIntervalClass,
        Name.Method(List("getEnd")),
        List(Pattern.EmptyAnonTuple)
    )
    
}