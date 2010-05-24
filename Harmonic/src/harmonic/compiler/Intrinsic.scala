package harmonic.compiler

import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition

case class Intrinsic(global: Global) {
    
    def requireLoadedOrLoadable(cls: Class[_]) = {
        val className = Name.Class(cls)
        global.requireLoadedOrLoadable(InterPosition.forClass(cls), className)
        global.csym(className)
    }
    
    def argPats(types: Type.Ref*) = {
        types.zipWithIndex.toList.map { case (ty, idx) =>
            Pattern.Var(new VarSymbol.Local(
                NoPosition, 
                Modifier.Set.empty,
                Name.LocalVar("arg%d".format(idx)),
                ty
            ))
        }
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
        
        numericTypes.foreach(requireLoadedOrLoadable)
        
        for(leftClass <- numericTypes; rightClass <- numericTypes) {
            val returnIndex = (
                numericTypes.indexOf(leftClass) max
                numericTypes.indexOf(rightClass) max
                numericTypes.indexOf(classOf[java.lang.Integer])
            )
            val returnClass = numericTypes(returnIndex)
            val leftTy = Type.Class(leftClass)
            val rightTy = Type.Class(rightClass)
            val returnTy = Type.Class(returnClass)
            for((interName, javaName) <- mathOps) {
                global.addIntrinsic(
                    new MethodSymbol(
                        pos = InterPosition.forClass(classOf[Intrinsic]),
                        modifiers = Modifier.Set.empty,
                        kind = MethodKind.Java(
                            MethodKind.JavaStatic,
                            classOf[IntrinsicMathGen],
                            javaName,
                            Array(leftClass, rightClass),
                            returnClass
                        ),
                        className = leftTy.name,
                        name = interName, 
                        MethodSignature(returnTy, argPats(rightTy))
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
        val blockClass = classOf[harmonic.lang.Block[_, _]]
        
        val booleanSym = requireLoadedOrLoadable(booleanClass)
        val voidSym = requireLoadedOrLoadable(voidClass)
        val objectSym = requireLoadedOrLoadable(objectClass)
        val iterableSym = requireLoadedOrLoadable(iterableClass)
        val blockSym = requireLoadedOrLoadable(blockClass)
        
        val booleanTy = Type.Class(booleanClass)
        val voidTy = Type.Class(voidClass)
        val objectTy = Type.Class(objectClass)
        val iterableTy = Type.Class(iterableClass)
        
        def controlFlow(
            mthdName: String,
            argumentClasses: Array[Class[_]],
            returnClass: Class[_]
        ) = {
            MethodKind.Java(
                MethodKind.JavaStatic,
                classOf[IntrinsicControlFlow],
                mthdName,
                argumentClasses,
                returnClass
            )
        }
        
        def blockTy(
            returnTy: Type.Ref,
            argumentTy: Type.Ref
        ) = {
            Type.Class(
                Name.Class(blockClass),
                List(
                    Type.TypeArg(Name.BlockR, TcEq, returnTy),
                    Type.TypeArg(Name.BlockA, TcEq, argumentTy)
                )
            )
        }
        
        // (boolean) if {...}
        global.addIntrinsic(
            new MethodSymbol(
                pos = InterPosition.forClass(classOf[Intrinsic]),
                modifiers = Modifier.Set.empty,
                kind = controlFlow(
                    "if_",
                    Array(booleanClass, blockClass),
                    voidClass
                ),
                className = booleanTy.name,
                name = Name.Method(List("if")),
                MethodSignature(voidTy, argPats(blockTy(voidTy, voidTy)))
            )
        )
        
        // (Object) ifNull {...}
        global.addIntrinsic(
            new MethodSymbol(
                pos = InterPosition.forClass(classOf[Intrinsic]),
                modifiers = Modifier.Set.empty,
                kind = controlFlow(
                    "ifNull",
                    Array(objectClass, blockClass),
                    voidClass
                ),
                className = objectTy.name,
                name = Name.Method(List("ifNull")),
                MethodSignature(voidTy, argPats(blockTy(voidTy, voidTy)))
            )
        )

        // (boolean) if {...} else {...}
        global.addIntrinsic(
            new MethodSymbol(
                pos = InterPosition.forClass(classOf[Intrinsic]),
                modifiers = Modifier.Set.empty,
                kind = controlFlow(
                    "ifElse",
                    Array(booleanClass, blockClass, blockClass),
                    objectClass
                ),
                className = booleanTy.name,
                name = Name.Method(List("if", "else")),
                MethodSignature(
                    returnTy = voidTy, /* ΧΧΧ Generic Method */
                    parameterPatterns = argPats(
                        blockTy(voidTy, voidTy),
                        blockTy(voidTy, voidTy)
                    )
                )
            )
        )
        
        // (Object) ifNull {...} else {...}
        global.addIntrinsic(
            new MethodSymbol(
                pos = InterPosition.forClass(classOf[Intrinsic]),
                modifiers = Modifier.Set.empty,
                kind = controlFlow(
                    "ifNullElse",
                    Array(objectClass, blockClass, blockClass),
                    objectClass
                ),
                className = objectTy.name,
                name = Name.Method(List("ifNull", "else")),
                MethodSignature(
                    returnTy = voidTy, /* ΧΧΧ Generic Method */
                    parameterPatterns = argPats(
                        blockTy(voidTy, voidTy), 
                        blockTy(voidTy, voidTy)
                    )
                )
            )
        )
        
        // (Iterable<T>) forEach { (T i) -> ... }
        val typeT = Type.Member(iterableSym.thisSym.toPath, Name.Member(iterableTy.name, "T"))
        global.addIntrinsic(
            new MethodSymbol(
                pos = InterPosition.forClass(classOf[Intrinsic]),
                modifiers = Modifier.Set.empty,
                kind = controlFlow(
                    "forEach",
                    Array(iterableClass, blockClass),
                    voidClass
                ),
                className = iterableTy.name,
                name = Name.Method(List("forEach")),
                MethodSignature(
                    returnTy = voidTy,
                    parameterPatterns = argPats(
                        blockTy(voidTy, typeT)
                    )
                )
            )
        )

        // (Block<Boolean,_>) while { ... }
        global.addIntrinsic(
            new MethodSymbol(
                pos = InterPosition.forClass(classOf[Intrinsic]),
                modifiers = Modifier.Set.empty,
                kind = controlFlow(
                    "while_",
                    Array(blockClass, blockClass),
                    objectClass
                ),
                className = Name.Class(blockClass),
                name = Name.Method(List("while")),
                MethodSignature(
                    returnTy = voidTy, 
                    parameterPatterns = argPats(
                        blockTy(voidTy, voidTy)
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