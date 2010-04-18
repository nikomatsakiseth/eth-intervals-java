package inter.compiler

object Intrinsic {
    
    def ensureLoadable(state: CompilationState, cls: Class[_]) {
        if(!state.loadedOrLoadable(Name.Qual(cls)))
            state.reporter.report(InterPosition.forClass(cls), "cannot.find.class", cls.toString)        
    }
    
    // ___ IntrinsicMath ____________________________________________________
    
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
        (Name.Method(List("+")), "plus"),
        (Name.Method(List("-")), "minus"),
        (Name.Method(List("/")), "divide"),
        (Name.Method(List("*")), "times")
    )
    
    private[this] def addMathTo(state: CompilationState) = {
        
        numericTypes.foreach(ensureLoadable(state, _))
        
        for(leftClass <- numericTypes; rightClass <- numericTypes) {
            val returnIndex = Math.max(
                Math.max(
                    numericTypes.indexOf(leftClass), 
                    numericTypes.indexOf(rightClass)
                ),
                numericTypes.indexOf(classOf[java.lang.Integer])
            )
            val returnClass = numericTypes(returnIndex)
            val leftTy = Type.Class(leftClass)
            val rightTy = Type.Class(rightClass)
            val returnTy = Type.Class(returnClass)
            for((interName, javaName) <- mathOps) {
                val msym = new Symbol.Method(
                    kind = Symbol.IntrinsicMath(javaName, leftClass, rightClass, returnClass),
                    name = interName, 
                    Symbol.MethodSignature(
                        returnTy = returnTy,
                        receiverTy = leftTy,
                        parameterPatterns = List(Pattern.Var(Name.Var("arg"), rightTy))
                    )
                )
                state.addIntrinsic(leftTy, msym)
            }
        }
        
    }
    
    // ___ General __________________________________________________________
    
    def addTo(state: CompilationState) = {
        addMathTo(state)
    }

}