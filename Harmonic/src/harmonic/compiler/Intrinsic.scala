package harmonic.compiler

import ch.ethz.intervals._
import ch.ethz.intervals.guard._
import harmonic.lang.Mutable
import harmonic.lang.StaticCheck
import com.smallcultfollowing.lathos.Lathos
import java.lang.reflect
import java.lang.annotation
import Util._

object Intrinsic {
    
    // ___ Extra Annotations ________________________________________________
    //
    // Rather than invoking foo.getAnnotation(), invoke foo.getHAnnotation(),
    // which may use default values.
    
    class AnnotHandler(cls: Class[_ <: annotation.Annotation], values: Map[String, Object])
    extends reflect.InvocationHandler {
        def invoke(proxy: Object, method: reflect.Method, args: Array[Object]): Object = {
            if(method.getName == "annotationType") cls
            else if(method.getName == "toString") "%s(%s)".format(cls.getSimpleName, values.mkString("; "))
            else values(method.getName)
        }
    }
    
    def at[C <: annotation.Annotation](cls: Class[C], values: Map[String, Object]): C = {
        val proxy = reflect.Proxy.newProxyInstance(
            cls.getClassLoader, Array(cls), new AnnotHandler(cls, values)
        )
        cls.cast(proxy)
    }
    
    private[this] val extraAnnot = Map[reflect.AnnotatedElement, List[annotation.Annotation]](
        
        classOf[RoInterval].getDeclaredMethod("getStart") -> List(
            at(classOf[Mutable], Map("value" -> Name.FinalMember.toAnnString))
        ),

        classOf[Interval].getDeclaredMethod("getStart") -> List(
            at(classOf[Mutable], Map("value" -> Name.FinalMember.toAnnString))
        ),
        
        classOf[RoInterval].getDeclaredMethod("getEnd") -> List(
            at(classOf[Mutable], Map("value" -> Name.FinalMember.toAnnString))
        ),

        classOf[Interval].getDeclaredMethod("getEnd") -> List(
            at(classOf[Mutable], Map("value" -> Name.FinalMember.toAnnString))
        ),
        
        classOf[RacyGuard].getDeclaredField("Racy") -> List(
            at(classOf[StaticCheck], Map())
        ),
        
        classOf[FinalGuard].getDeclaredField("Final") -> List(
            at(classOf[StaticCheck], Map())
        )
        
    )
    
    case class HarmonicAnnotated(obj: reflect.AnnotatedElement) {
        def getHAnnotation[C <: annotation.Annotation](cls: Class[C]): Option[C] = {
            Lathos.context.indent("getHAnnotation(", obj, ", ", cls, ")") {
                obj.getAnnotation(cls) match {
                    case null => {
                        extraAnnot.get(obj).flatMap { extras =>
                            Lathos.context.log("extras = ", extras)
                            Lathos.context.log(cls, " isInst = ", extras.map(cls.isInstance))
                            extras.find(cls.isInstance(_)).map(cls.cast(_))                            
                        }
                    }
                    case ann => Some(ann)
                }                
            }
        }
        
        def hasHAnnotation[C <: annotation.Annotation](cls: Class[C]): Boolean = {
            getHAnnotation(cls).isDefined
        }
    }
    
    implicit def toHarmonicAnnotated(obj: reflect.AnnotatedElement) = HarmonicAnnotated(obj)
    
}

case class Intrinsic(global: Global) {
    implicit val implicitGlobal = global
    
    class IntrinsicMethodSymbol(
        implClass: Class[_],
        javaName: String,
        argClasses: Array[Class[_]],
        returnClass: Class[_],
        val className: Name.Class,
        val name: Name.Method,
        val msig: MethodSignature[Pattern.Ref],
        val requirements: List[inference.Fact] = Nil,
        val ensures: List[inference.Fact] = Nil,
        val guardPath: Path = Path.RacyGuard
    ) extends VirtualMethodSymbol {
        val pos = InterPosition.forClass(classOf[Intrinsic])
        val modifiers = Modifier.Set.empty
        val kind = MethodKind.Java(MethodKind.JavaStatic, implClass, javaName, argClasses, returnClass)
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
                    new IntrinsicMethodSymbol(
                        implClass   = classOf[IntrinsicMathGen], 
                        javaName    = javaName, 
                        argClasses  = Array(leftClass, rightClass), 
                        returnClass = returnClass,
                        className   = leftTy.name,
                        name        = interName,
                        msig        = MethodSignature(returnTy, List(Pattern.Var(Name.LocalVar("arg"), rightTy)))
                    )
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
            new IntrinsicMethodSymbol(
                implClass   = classOf[IntrinsicControlFlow], 
                javaName    = "if_", 
                argClasses  = Array(booleanClass, templateClass), 
                returnClass = voidClass,
                className   = booleanTy.name,
                name        = Name.Method(List("if")),
                msig        = MethodSignature(voidTy, List(
                    Pattern.Var(Name.LocalVar("ifTmpl"), templateTy(voidTy, voidTy))
                ))
            )
        )
        
        // (Object) ifNull {...}
        global.addIntrinsic(
            new IntrinsicMethodSymbol(
                implClass   = classOf[IntrinsicControlFlow], 
                javaName    = "ifNull", 
                argClasses  = Array(objectClass, templateClass), 
                returnClass = voidClass,
                className   = objectTy.name,
                name        = Name.Method(List("ifNull")),
                msig        = MethodSignature(voidTy, List(
                    Pattern.Var(Name.LocalVar("ifTmpl"), templateTy(voidTy, voidTy))
                ))
            )
        )

        // (boolean) if {...} else {...}
        global.addIntrinsic(
            new IntrinsicMethodSymbol(
                implClass   = classOf[IntrinsicControlFlow], 
                javaName    = "ifElse", 
                argClasses  = Array(booleanClass, templateClass, templateClass), 
                returnClass = objectClass,
                className   = booleanTy.name,
                name        = Name.Method(List("if", "else")),
                msig        = MethodSignature(voidTy, List( /* TODO Generic Method */
                    Pattern.Var(Name.LocalVar("ifTmpl"), templateTy(voidTy, voidTy)),
                    Pattern.Var(Name.LocalVar("elseTmpl"), templateTy(voidTy, voidTy))
                ))
            )
        )
        
        // (Object) ifNull {...} else {...}
        global.addIntrinsic(
            new IntrinsicMethodSymbol(
                implClass   = classOf[IntrinsicControlFlow], 
                javaName    = "ifNullElse", 
                argClasses  = Array(objectClass, templateClass, templateClass), 
                returnClass = objectClass,
                className   = objectTy.name,
                name        = Name.Method(List("ifNull", "else")),
                msig        = MethodSignature(voidTy, List( /* TODO Generic Method */
                    Pattern.Var(Name.LocalVar("ifTmpl"), templateTy(voidTy, voidTy)),
                    Pattern.Var(Name.LocalVar("elseTmpl"), templateTy(voidTy, voidTy))
                ))
            )
        )
        
        // (Iterable<T>) forEach { (T i) -> ... }
        val typeT = Type.Member(Path.This, Name.Member(iterableTy.name, "T"))
        global.addIntrinsic(
            new IntrinsicMethodSymbol(
                implClass   = classOf[IntrinsicControlFlow], 
                javaName    = "forEach", 
                argClasses  = Array(iterableClass, templateClass),
                returnClass = voidClass,
                className   = iterableTy.name,
                name        = Name.Method(List("forEach")),
                msig        = MethodSignature(voidTy, List( /* TODO Generic Method */
                    Pattern.Var(Name.LocalVar("eachTmpl"), templateTy(voidTy, typeT))
                ))
            )
        )

        // (Block<Boolean,_>) while { ... }
        global.addIntrinsic(
            new IntrinsicMethodSymbol(
                implClass   = classOf[IntrinsicControlFlow], 
                javaName    = "while_", 
                argClasses  = Array(templateClass, templateClass),
                returnClass = objectClass,
                className   = Name.Class(templateClass),
                name        = Name.Method(List("while")),
                msig        = MethodSignature(voidTy, List( /* TODO Generic Method */
                    Pattern.Var(Name.LocalVar("bodyTmpl"), templateTy(voidTy, voidTy))
                ))
            )
        )    

    }
    
    // ___ General __________________________________________________________
    
    def add() = {
        inlineInterval("Intrinsic Creation") { inter =>
            addMathTo(inter)
            addControlFlow(inter)            
            ensureLoadable(classOf[ch.ethz.intervals.RoPoint])
            ensureLoadable(classOf[ch.ethz.intervals.Point])
            ensureLoadable(classOf[ch.ethz.intervals.RoInterval])
            ensureLoadable(classOf[ch.ethz.intervals.AsyncInterval])
            ensureLoadable(classOf[ch.ethz.intervals.InlineInterval])
            ensureLoadable(classOf[ch.ethz.intervals.Interval])
            ensureLoadable(classOf[ch.ethz.intervals.guard.FinalGuard])
            ensureLoadable(classOf[ch.ethz.intervals.guard.RacyGuard])
        }
    }

}