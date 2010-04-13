package inter.compiler

object Intrinsic {
    
    private[this] def addMathTo(state: CompilationState) = {
        
        val integralTypes = List[Class[_]](
            classOf[java.lang.Byte],
            classOf[java.lang.Short],
            classOf[java.lang.Integer],
            classOf[java.lang.Long]
        )
        
        val floatTypes = List[Class[_]](
            classOf[java.lang.Float],
            classOf[java.lang.Double]
        )
        
        val numericTypes = integralTypes ++ floatTypes
        
        val mathOps = List(
            Name.Method(List("+")),
            Name.Method(List("-")),
            Name.Method(List("/")),
            Name.Method(List("*"))
        )
        
        for(tl <- numericTypes; tr <- numericTypes) {
            val leftTy = Type.Class(Name.Qual(tl), List())
            val rightTy = Type.Class(Name.Qual(tr), List())
            val returnTy = 
                if(numericTypes.indexOf(tl) < numericTypes.indexOf(tr)) rightTy
                else leftTy
            for(op <- mathOps) {
                val msym = new Symbol.Method(
                    name = op, 
                    returnTy = returnTy,
                    receiver = Symbol.VarPattern(Name.ThisVar, leftTy),
                    parameterPatterns = List(Symbol.VarPattern(Name.Var("arg"), rightTy)))
                state.addIntrinsic(leftTy, msym)
            }
        }
        
    }
    
    def addTo(state: CompilationState) = {
        addMathTo(state)
    }
    
}