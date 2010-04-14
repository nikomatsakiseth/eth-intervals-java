package inter.compiler

object Intrinsic {
    
    val resolveClases = List(
        classOf[java.lang.Byte],
        classOf[java.lang.Short],
        classOf[java.lang.Integer],
        classOf[java.lang.Long],
        classOf[java.lang.Float],
        classOf[java.lang.Double]
    )
    
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
                    receiver = Pattern.Var(Name.ThisVar, leftTy),
                    parameterPatterns = List(Pattern.Var(Name.Var("arg"), rightTy)))
                state.addIntrinsic(leftTy, msym)
            }
        }
        
    }
    
    def addTo(state: CompilationState) = {
        resolveClases.foreach { cls =>
            if(!state.loadedOrLoadable(Name.Qual(cls)))
                state.reporter.report(InterPosition.forClass(cls), "cannot.find.class", cls.toString)
        }
        
        addMathTo(state)
    }
    
}