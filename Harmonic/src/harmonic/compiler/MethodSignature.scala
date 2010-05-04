package harmonic.compiler

case class MethodSignature[+P <: Pattern.Anon](
    val returnTy: Type.Ref,
    val receiverTy: Type.Ref,
    val parameterPatterns: List[P]        
) {
    def receiverAnonPattern: Pattern.AnonVar = Pattern.SubstdVar(receiverTy)
    def thisPattern: Pattern.Var = Pattern.Var(Name.ThisLocal, receiverTy)
}
