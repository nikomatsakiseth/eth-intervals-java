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
                state.addIntrinsic(leftTy.name, msym)
            }
        }
        
    }
    
    // ___ IntrinsicControlFlow _____________________________________________
    
    private[this] def addControlFlow(state: CompilationState) = {
        
        val booleanClass = classOf[java.lang.Boolean]
        val voidClass = classOf[java.lang.Void]
        val objectClass = classOf[java.lang.Object]
        val iterableClass = classOf[java.lang.Iterable[_]]
        val templateClass = classOf[inter.lang.IntervalTemplate[_, _]]
        
        ensureLoadable(state, booleanClass)
        ensureLoadable(state, voidClass)
        ensureLoadable(state, objectClass)
        ensureLoadable(state, iterableClass)
        ensureLoadable(state, templateClass)
        
        val booleanTy = Type.Class(booleanClass)
        val voidTy = Type.Class(voidClass)
        val objectTy = Type.Class(objectClass)
        val iterableTy = Type.Class(iterableClass)
        
        def templateTy(
            returnTy: Type.Ref,
            argumentTy: Type.Ref
        ) = {
            Type.Class(
                Name.Qual(templateClass),
                List(
                    Type.TypeArg(Name.RVar, TcEq, returnTy),
                    Type.TypeArg(Name.AVar, TcEq, argumentTy)
                )
            )
        }
        
        // (boolean) if {...}
        state.addIntrinsic(
            booleanTy.name,
            new Symbol.Method(
                kind = Symbol.IntrinsicControlFlow(
                    "if_",
                    Array(booleanClass, templateClass),
                    voidClass
                ),
                name = Name.Method(List("if")),
                Symbol.MethodSignature(
                    returnTy = voidTy,
                    receiverTy = booleanTy,
                    parameterPatterns = List(
                        Pattern.Var(Name.Var("ifTmpl"), templateTy(voidTy, voidTy))
                    )
                )
            )
        )
        
        // (Object) ifNull {...}
        state.addIntrinsic(
            objectTy.name,
            new Symbol.Method(
                kind = Symbol.IntrinsicControlFlow(
                    "ifNull",
                    Array(objectClass, templateClass),
                    voidClass
                ),
                name = Name.Method(List("ifNull")),
                Symbol.MethodSignature(
                    returnTy = voidTy,
                    receiverTy = objectTy,
                    parameterPatterns = List(
                        Pattern.Var(Name.Var("ifTmpl"), templateTy(voidTy, voidTy))
                    )
                )
            )
        )

        // (boolean) if {...} else {...}
        state.addIntrinsic(
            booleanTy.name,
            new Symbol.Method(
                kind = Symbol.IntrinsicControlFlow(
                    "ifElse",
                    Array(booleanClass, templateClass, templateClass),
                    objectClass
                ),
                name = Name.Method(List("if", "else")),
                Symbol.MethodSignature(
                    returnTy = voidTy, /* ΧΧΧ Generic Method */
                    receiverTy = booleanTy,
                    parameterPatterns = List(
                        Pattern.Var(Name.Var("ifTmpl"), templateTy(voidTy, voidTy)),
                        Pattern.Var(Name.Var("elseTmpl"), templateTy(voidTy, voidTy))
                    )
                )
            )
        )
        
        // (Object) ifNull {...} else {...}
        state.addIntrinsic(
            objectTy.name,
            new Symbol.Method(
                kind = Symbol.IntrinsicControlFlow(
                    "ifNullElse",
                    Array(objectClass, templateClass, templateClass),
                    objectClass
                ),
                name = Name.Method(List("ifNull", "else")),
                Symbol.MethodSignature(
                    returnTy = voidTy, /* ΧΧΧ Generic Method */
                    receiverTy = objectTy,
                    parameterPatterns = List(
                        Pattern.Var(Name.Var("ifTmpl"), templateTy(voidTy, voidTy)),
                        Pattern.Var(Name.Var("elseTmpl"), templateTy(voidTy, voidTy))
                    )
                )
            )
        )
        
        // (Iterable<T>) forEach { (T i) -> ... }
        val typeT = Type.Var(Path.This, Name.Var("T"))
        state.addIntrinsic(
            iterableTy.name,
            new Symbol.Method(
                kind = Symbol.IntrinsicControlFlow(
                    "forEach",
                    Array(iterableClass, templateClass),
                    voidClass
                ),
                name = Name.Method(List("forEach")),
                Symbol.MethodSignature(
                    returnTy = voidTy,
                    receiverTy = iterableTy,
                    parameterPatterns = List(
                        Pattern.Var(
                            Name.Var("eachTmpl"), 
                            templateTy(voidTy, typeT)
                        )
                    )
                )
            )
        )

        // (IntervalTemplate<Boolean,_>) while { ... }
        state.addIntrinsic(
            Name.Qual(templateClass),
            new Symbol.Method(
                kind = Symbol.IntrinsicControlFlow(
                    "while_",
                    Array(templateClass, templateClass),
                    objectClass
                ),
                name = Name.Method(List("while")),
                Symbol.MethodSignature(
                    returnTy = voidTy, 
                    receiverTy = templateTy(booleanTy, voidTy),
                    parameterPatterns = List(
                        Pattern.Var(Name.Var("bodyTmpl"), templateTy(voidTy, voidTy))
                    )
                )
            )
        )    

    }
    
    // ___ General __________________________________________________________
    
    def addTo(state: CompilationState) = {
        addMathTo(state)
        addControlFlow(state)
    }

}