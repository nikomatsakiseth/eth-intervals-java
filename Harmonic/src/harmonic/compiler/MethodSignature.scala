package harmonic.compiler

case class MethodSignature[P <: Pattern.Any](
    val returnTy: Type,
    val parameterPatterns: List[P]
) {
    def flatParamTypes = parameterPatterns.flatMap(_.varTys)
    
    def toAnon = MethodSignature(returnTy, parameterPatterns.map(_.toAnon))
    
    override def toString = "[(%s): %s]".format(
        parameterPatterns.mkString(", "), returnTy
    )
}