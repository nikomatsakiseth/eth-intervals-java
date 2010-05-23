package harmonic.compiler

case class MethodSignature[+P <: Pattern.Anon](
    val returnTy: Type.Ref,
    val parameterPatterns: List[P]        
)