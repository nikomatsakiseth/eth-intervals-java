package harmonic.compiler

import ch.ethz.intervals._
import Util._

case class Intrinsic(global: Global) {
    implicit val implicitGlobal = global
    
    private[this] def initReqs(msym: MethodSymbol) = {
        msym.Requirements.v = Nil
        msym.Ensures.v = Nil
        msym
    }
    
    def ensureLoadable(cls: Class[_]) {
        global.requireLoadedOrLoadable(InterPosition.forClass(cls), Name.Class(cls))
    }
    
    // ___ Extra Members ____________________________________________________
    //
    // This is used to add ghosts to Object.
    
    val objectPos = InterPosition.forClass(classOf[Object])
    
    def extraVarMembers(name: Name.Class): Array[SymTab.Entry] = {
        name match {
            case Name.ObjectClass => Array(SymTab.Ghost(Name.Wr), SymTab.Ghost(Name.Init))
            case _ => Array()
        }
    }
    
    def extraGhostSymbols(name: Name.Class): Array[GhostSymbol] = {
        name match {
            case Name.ObjectClass => Array(
                new GhostSymbol(objectPos, Name.Wr, Name.GuardClass),
                new GhostSymbol(objectPos, Name.Init, Name.IntervalClass)
            )
            case _ => Array()
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
    
    private[this] def addMathTo(inter: Interval) = {
        
        numericTypes.foreach(ensureLoadable)
        
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
                    initReqs(new MethodSymbol(
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
                        elaborate = inter,
                        msig = MethodSignature(
                            returnTy = returnTy,
                            parameterPatterns = List(Pattern.Var(Name.LocalVar("arg"), rightTy))
                        )
                    ))
                )
            }
        }
        
    }
    
    // ___ IntrinsicControlFlow _____________________________________________
    
    private[this] def addControlFlow(inter: Interval) = {
        
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
        
        def templateTy(
            returnTy: Type,
            argumentTy: Type
        ) = {
            Type.Class(
                Name.Class(templateClass),
                List(
                    Type.TypeArg(Name.BlockR, TcSub, returnTy),
                    Type.TypeArg(Name.BlockA, TcSup, argumentTy)
                )
            )
        }
        
        // (boolean) if {...}
        global.addIntrinsic(
            initReqs(new MethodSymbol(
                pos = InterPosition.forClass(classOf[Intrinsic]),
                modifiers = Modifier.Set.empty,
                kind = controlFlow(
                    "if_",
                    Array(booleanClass, templateClass),
                    voidClass
                ),
                className = booleanTy.name,
                name = Name.Method(List("if")),
                elaborate = inter,
                msig = MethodSignature(
                    returnTy = voidTy,
                    parameterPatterns = List(
                        Pattern.Var(Name.LocalVar("ifTmpl"), templateTy(voidTy, voidTy))
                    )
                )
            ))
        )
        
        // (Object) ifNull {...}
        global.addIntrinsic(
            initReqs(new MethodSymbol(
                pos = InterPosition.forClass(classOf[Intrinsic]),
                modifiers = Modifier.Set.empty,
                kind = controlFlow(
                    "ifNull",
                    Array(objectClass, templateClass),
                    voidClass
                ),
                className = objectTy.name,
                name = Name.Method(List("ifNull")),
                elaborate = inter,
                msig = MethodSignature(
                    returnTy = voidTy,
                    parameterPatterns = List(
                        Pattern.Var(Name.LocalVar("ifTmpl"), templateTy(voidTy, voidTy))
                    )
                )
            ))
        )

        // (boolean) if {...} else {...}
        global.addIntrinsic(
            initReqs(new MethodSymbol(
                pos = InterPosition.forClass(classOf[Intrinsic]),
                modifiers = Modifier.Set.empty,
                kind = controlFlow(
                    "ifElse",
                    Array(booleanClass, templateClass, templateClass),
                    objectClass
                ),
                className = booleanTy.name,
                name = Name.Method(List("if", "else")),
                elaborate = inter,
                msig = MethodSignature(
                    returnTy = voidTy, /* ΧΧΧ Generic Method */
                    parameterPatterns = List(
                        Pattern.Var(Name.LocalVar("ifTmpl"), templateTy(voidTy, voidTy)),
                        Pattern.Var(Name.LocalVar("elseTmpl"), templateTy(voidTy, voidTy))
                    )
                )
            ))
        )
        
        // (Object) ifNull {...} else {...}
        global.addIntrinsic(
            initReqs(new MethodSymbol(
                pos = InterPosition.forClass(classOf[Intrinsic]),
                modifiers = Modifier.Set.empty,
                kind = controlFlow(
                    "ifNullElse",
                    Array(objectClass, templateClass, templateClass),
                    objectClass
                ),
                className = objectTy.name,
                name = Name.Method(List("ifNull", "else")),
                elaborate = inter,
                msig = MethodSignature(
                    returnTy = voidTy, /* ΧΧΧ Generic Method */
                    parameterPatterns = List(
                        Pattern.Var(Name.LocalVar("ifTmpl"), templateTy(voidTy, voidTy)),
                        Pattern.Var(Name.LocalVar("elseTmpl"), templateTy(voidTy, voidTy))
                    )
                )
            ))
        )
        
        // (Iterable<T>) forEach { (T i) -> ... }
        val typeT = Type.Member(Path.This, Name.Member(iterableTy.name, "T"))
        global.addIntrinsic(
            initReqs(new MethodSymbol(
                pos = InterPosition.forClass(classOf[Intrinsic]),
                modifiers = Modifier.Set.empty,
                kind = controlFlow(
                    "forEach",
                    Array(iterableClass, templateClass),
                    voidClass
                ),
                className = iterableTy.name,
                name = Name.Method(List("forEach")),
                elaborate = inter,
                msig = MethodSignature(
                    returnTy = voidTy,
                    parameterPatterns = List(
                        Pattern.Var(
                            Name.LocalVar("eachTmpl"), 
                            templateTy(voidTy, typeT)
                        )
                    )
                )
            ))
        )

        // (Block<Boolean,_>) while { ... }
        global.addIntrinsic(
            initReqs(new MethodSymbol(
                pos = InterPosition.forClass(classOf[Intrinsic]),
                modifiers = Modifier.Set.empty,
                kind = controlFlow(
                    "while_",
                    Array(templateClass, templateClass),
                    objectClass
                ),
                className = Name.Class(templateClass),
                name = Name.Method(List("while")),
                elaborate = inter,
                msig = MethodSignature(
                    returnTy = voidTy, 
                    parameterPatterns = List(
                        Pattern.Var(Name.LocalVar("bodyTmpl"), templateTy(voidTy, voidTy))
                    )
                )
            ))
        )    

    }
    
    // ___ General __________________________________________________________
    
    def add() = {
        inlineInterval("Intrinsic Creation") { inter =>
            addMathTo(inter)
            addControlFlow(inter)            
        }
    }

}