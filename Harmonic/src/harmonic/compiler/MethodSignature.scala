package harmonic.compiler

case class MethodSignature[+P <: Pattern.Anon](
    val returnTy: Type.Ref,
    val parameterPatterns: List[P]
) {
    def flatParamTypes = parameterPatterns.flatMap(_.varTys)
    
    override def toString = "[(%s): %s]".format(
        parameterPatterns.mkString(", "), returnTy
    )
}