package harmonic.compiler

case class Intrinsic(state: CompilationState) {
    
    def ensureLoadable(cls: Class[_]) {
        state.requireLoadedOrLoadable(InterPosition.forClass(cls), Name.Class(cls))
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
    
    private[this] def addMathTo() = {
        
        numericTypes.foreach(ensureLoadable)
        
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
                state.addIntrinsic(
                    new MethodSymbol(
                        pos = InterPosition.forClass(classOf[Intrinsic]),
                        modifiers = Modifier.Set.empty,
                        kind = Symbol.IntrinsicMath(javaName, leftClass, rightClass, returnClass),
                        clsName = leftTy.name,
                        name = interName, 
                        MethodSignature(
                            returnTy = returnTy,
                            receiverTy = leftTy,
                            parameterPatterns = List(Pattern.Var(Name.LocalVar("arg"), rightTy))
                        )
                    )
                )
            }
        }
        
    }
    
    // ___ IntrinsicControlFlow _____________________________________________
    
    private[this] def addControlFlow() = {
        
        val booleanClass = classOf[java.lang.Boolean]
        val voidClass = classOf[java.lang.Void]
        val objectClass = classOf[java.lang.Object]
        val iterableClass = classOf[java.lang.Iterable[_]]
        val templateClass = classOf[harmonic.lang.Block[_, _]]
        
        ensureLoadable(booleanClass)
        ensureLoadable(voidClass)
        ensureLoadable(objectClass)
        ensureLoadable(iterableClass)
        ensureLoadable(templateClass)
        
        val booleanTy = Type.Class(booleanClass)
        val voidTy = Type.Class(voidClass)
        val objectTy = Type.Class(objectClass)
        val iterableTy = Type.Class(iterableClass)
        
        def controlFlow(
            mthdName: String,
            argumentClasses: Array[Class[_]],
            resultClass: Class[_]
        ) = Symbol.IntrinsicStatic(classOf[IntrinsicControlFlow], mthdName, argumentClasses, resultClass)
        
        def templateTy(
            returnTy: Type.Ref,
            argumentTy: Type.Ref
        ) = {
            Type.Class(
                Name.Class(templateClass),
                List(
                    Type.TypeArg(Name.BlockR, TcEq, returnTy),
                    Type.TypeArg(Name.BlockA, TcEq, argumentTy)
                )
            )
        }
        
        // (boolean) if {...}
        state.addIntrinsic(
            new MethodSymbol(
                pos = InterPosition.forClass(classOf[Intrinsic]),
                modifiers = Modifier.Set.empty,
                kind = controlFlow(
                    "if_",
                    Array(booleanClass, templateClass),
                    voidClass
                ),
                clsName = booleanTy.name,
                name = Name.Method(List("if")),
                MethodSignature(
                    returnTy = voidTy,
                    receiverTy = booleanTy,
                    parameterPatterns = List(
                        Pattern.Var(Name.LocalVar("ifTmpl"), templateTy(voidTy, voidTy))
                    )
                )
            )
        )
        
        // (Object) ifNull {...}
        state.addIntrinsic(
            new MethodSymbol(
                pos = InterPosition.forClass(classOf[Intrinsic]),
                modifiers = Modifier.Set.empty,
                kind = controlFlow(
                    "ifNull",
                    Array(objectClass, templateClass),
                    voidClass
                ),
                clsName = objectTy.name,
                name = Name.Method(List("ifNull")),
                MethodSignature(
                    returnTy = voidTy,
                    receiverTy = objectTy,
                    parameterPatterns = List(
                        Pattern.Var(Name.LocalVar("ifTmpl"), templateTy(voidTy, voidTy))
                    )
                )
            )
        )

        // (boolean) if {...} else {...}
        state.addIntrinsic(
            new MethodSymbol(
                pos = InterPosition.forClass(classOf[Intrinsic]),
                modifiers = Modifier.Set.empty,
                kind = controlFlow(
                    "ifElse",
                    Array(booleanClass, templateClass, templateClass),
                    objectClass
                ),
                clsName = booleanTy.name,
                name = Name.Method(List("if", "else")),
                MethodSignature(
                    returnTy = voidTy, /* ΧΧΧ Generic Method */
                    receiverTy = booleanTy,
                    parameterPatterns = List(
                        Pattern.Var(Name.LocalVar("ifTmpl"), templateTy(voidTy, voidTy)),
                        Pattern.Var(Name.LocalVar("elseTmpl"), templateTy(voidTy, voidTy))
                    )
                )
            )
        )
        
        // (Object) ifNull {...} else {...}
        state.addIntrinsic(
            new MethodSymbol(
                pos = InterPosition.forClass(classOf[Intrinsic]),
                modifiers = Modifier.Set.empty,
                kind = controlFlow(
                    "ifNullElse",
                    Array(objectClass, templateClass, templateClass),
                    objectClass
                ),
                clsName = objectTy.name,
                name = Name.Method(List("ifNull", "else")),
                MethodSignature(
                    returnTy = voidTy, /* ΧΧΧ Generic Method */
                    receiverTy = objectTy,
                    parameterPatterns = List(
                        Pattern.Var(Name.LocalVar("ifTmpl"), templateTy(voidTy, voidTy)),
                        Pattern.Var(Name.LocalVar("elseTmpl"), templateTy(voidTy, voidTy))
                    )
                )
            )
        )
        
        // (Iterable<T>) forEach { (T i) -> ... }
        val typeT = Type.Var(Path.This, Name.Member(iterableTy.name, "T"))
        state.addIntrinsic(
            new MethodSymbol(
                pos = InterPosition.forClass(classOf[Intrinsic]),
                modifiers = Modifier.Set.empty,
                kind = controlFlow(
                    "forEach",
                    Array(iterableClass, templateClass),
                    voidClass
                ),
                clsName = iterableTy.name,
                name = Name.Method(List("forEach")),
                MethodSignature(
                    returnTy = voidTy,
                    receiverTy = iterableTy,
                    parameterPatterns = List(
                        Pattern.Var(
                            Name.LocalVar("eachTmpl"), 
                            templateTy(voidTy, typeT)
                        )
                    )
                )
            )
        )

        // (Block<Boolean,_>) while { ... }
        state.addIntrinsic(
            new MethodSymbol(
                pos = InterPosition.forClass(classOf[Intrinsic]),
                modifiers = Modifier.Set.empty,
                kind = controlFlow(
                    "while_",
                    Array(templateClass, templateClass),
                    objectClass
                ),
                clsName = Name.Class(templateClass),
                name = Name.Method(List("while")),
                MethodSignature(
                    returnTy = voidTy, 
                    receiverTy = templateTy(booleanTy, voidTy),
                    parameterPatterns = List(
                        Pattern.Var(Name.LocalVar("bodyTmpl"), templateTy(voidTy, voidTy))
                    )
                )
            )
        )    

    }
    
    // ___ General __________________________________________________________
    
    def add() = {
        addMathTo()
        addControlFlow()
    }

}