package harmonic.compiler

import ch.ethz.intervals._

import java.lang.reflect
import java.lang.reflect.{Modifier => jModifier}

import scala.collection.mutable

import com.smallcultfollowing.lathos.Lathos

import harmonic.lang.Ghosts
import harmonic.lang.Ghost

import Util._

class ClassFromReflection(
    val name: Name.Class,
    val global: Global,
    val cls: java.lang.Class[_]
) extends ClassFromCompiledSource {
    
    def isPrivate(member: reflect.Member) = (member.getModifiers & reflect.Modifier.PRIVATE) != 0
    def isDeclaredByCls(member: reflect.Member) = (member.getDeclaringClass == cls)
    def isSuitable(member: reflect.Member) = isDeclaredByCls(member) && !isPrivate(member)
    
    protected[this] def loadData(inter: Interval) = Data(
        modifiers = Modifier.forClass(cls),
        
        superClassNames = {
            val allNames = {
                if((cls.getModifiers & reflect.Modifier.INTERFACE) != 0)
                    cls.getInterfaces.toList.map(Name.Class) :+ Name.ObjectClass
                else
                    (cls.getSuperclass :: cls.getInterfaces.toList).filter(_ != null).map(Name.Class)
            }
            Lathos.context.log(name, ".superClassNames = ", allNames)
            allNames.foreach(global.requireLoadedOrLoadable(pos, _))
            allNames
        },
        
        varMembers = List[Array[SymTab.Entry]](
            Intrinsic(global).extraVarMembers(name),
            ghostSymTabEntries(cls.getAnnotation(classOf[Ghosts])),
            cls.getDeclaredFields.map(fieldSymTabEntry),
            cls.getTypeParameters.map(typeParamSymTabEntry)
        ).flatten,
        
        constructors = {
            val ctors = cls.getConstructors
            if(!ctors.isEmpty) {
                ctors.map(ctorSymbol(inter)).toList            
            } else { // gin up an empty constructor for interfaces:
                List(
                    initReqs(new MethodSymbol(
                        pos       = pos, 
                        modifiers = Modifier.Set.empty,
                        kind      = MethodKind.JavaDummyCtor,
                        className = name,
                        name      = Name.InitMethod,
                        elaborate = inter,
                        msig      = MethodSignature(Type.Void, List())
                    ))
                )
            }            
        },
        
        allMethodSymbols = {
            elimPrimitiveConflicts(
                elimCovariantReturns(cls.getDeclaredMethods)
                .filter(isSuitable)
                .map(methodSymbol(inter))
            )
        },
        
        allFieldSymbols = List(
            cls.getDeclaredFields.filter(isSuitable).map(fieldSymbol)
        ).flatten,
        
        allIntervalSymbols = Nil,
        
        allGhostSymbols = List(
            Intrinsic(global).extraGhostSymbols(name),
            ghostSymbols(cls.getAnnotation(classOf[Ghosts]))
        ).flatten,
        
        checkEnv = Env.empty(global)
    )
    
    private[this] def initReqs(msym: MethodSymbol) = {
        // TODO Honor @Requires and @Ensures annotations
        msym.Requirements.v = Nil
        msym.Ensures.v = Nil
        msym
    }
    
    private[this] def fieldSymTabEntry(fld: reflect.Field) = {
        val memberName = Name.Member(name, fld.getName)
        val modifiers = Modifier.forMember(fld)
        if(modifiers.isStatic) SymTab.StaticField(memberName)
        else SymTab.InstanceField(memberName)
    }
    
    private[this] def ghostSymTabEntries(ghosts: Ghosts): Array[SymTab.Entry] = {
        if(ghosts == null) Array()
        else for(g <- ghosts.value) yield SymTab.Ghost(Name.Member(name, g.name))
    }
    
    private[this] def typeParamSymTabEntry(tv: reflect.TypeVariable[_]) = {
        SymTab.Type(Name.Member(name, tv.getName))
    }
    
    private[this] def typeArg(pair: (Name.Member, reflect.Type)): Option[Type.TypeArg] = pair match {
        case (nm, wt: reflect.WildcardType) => { /* // FIXME: Allow multiple LB, UB? */
            val lbs = wt.getLowerBounds
            val ubs = wt.getUpperBounds
            if(!lbs.isEmpty) {
                Some(Type.TypeArg(nm, TcSup, typeRef(lbs(0))))
            } else if(!ubs.isEmpty) {
                Some(Type.TypeArg(nm, TcSub, typeRef(ubs(0))))
            } else None
        }
        case (nm, ty) => Some(Type.TypeArg(nm, TcEq, typeRef(ty)))
    }
    
    private[this] def typeRef(ty: reflect.Type): Type = ty match {
        case ty: Class[_] if ty.isArray => {
            val targ = typeArg(Name.ArrayElem, ty.getComponentType).get
            global.requireLoadedOrLoadable(pos, Name.ArrayClass)
            Type.Class(Name.ArrayClass, List(targ))            
        }
        case ty: Class[_] => {
            val name = Name.Class(ty)
            global.requireLoadedOrLoadable(pos, name)
            name.toType
        }
        case ty: reflect.GenericArrayType => {
            val targ = typeArg(Name.ArrayElem, ty.getGenericComponentType).get
            global.requireLoadedOrLoadable(pos, Name.ArrayClass)
            Type.Class(Name.ArrayClass, List(targ))
        }
        case ty: reflect.TypeVariable[_] => {
            ty.getGenericDeclaration match {
                case cls: java.lang.Class[_] => {
                    val name = Name.Class(cls)
                    Type.Member(Path.This, Name.Member(name, ty.getName))    
                }                                
                case _ => {
                    // TODO Method type variables... how can we do it?                    
                    Type.Member(Path.This, Name.Member(Name.ObjectClass, ty.getName))                                    
                }
            }
        }
        case ty: reflect.ParameterizedType => {
            val cls = ty.getRawType.asInstanceOf[Class[_]]
            val className = Name.Class(cls)
            val tparams = cls.getTypeParameters.toList.map(tv => Name.Member(className, tv.getName))
            val targs = tparams.zip(ty.getActualTypeArguments).flatMap(typeArg)
            Type.Class(className, targs)
        }
        case _ => throw new RuntimeException("Not here")
    }
    
    private[this] def fieldSymbol(fld: reflect.Field) = {
        new VarSymbol.Field(
            pos       = pos,
            modifiers = Modifier.forMember(fld),
            name      = Name.Member(name, fld.getName),
            ty        = typeRef(fld.getGenericType),
            kind      = FieldKind.Java(cls, fld.getName, fld.getType)
        )        
    }
    
    private[this] def ghostSymbol(ghost: Ghost) = {
        new GhostSymbol(
            pos,
            Name.Member(name, ghost.name),
            Name.Class(ghost.cls)
        )
    }
    
    private[this] def ghostSymbols(ghosts: Ghosts): Array[GhostSymbol] = {
        if(ghosts == null) Array()
        else for(g <- ghosts.value) yield ghostSymbol(g)
    }
    
    private[this] def paramPattern(pair: (reflect.Type, Int)) = {
        Pattern.Var(
            name = Name.LocalVar("arg"+pair._2),
            ty   = typeRef(pair._1)
        )
    }
    
    private[this] def ctorSymbol(inter: Interval)(mthd: reflect.Constructor[_]) = {
        initReqs(new MethodSymbol(
            pos       = pos,
            modifiers = Modifier.forMember(mthd),
            kind      = MethodKind.Java(
                MethodKind.JavaSpecial, 
                cls, 
                Name.InitMethod.javaName,
                mthd.getParameterTypes,
                classOf[Unit]
            ),
            className = name,
            name      = Name.InitMethod,
            elaborate = inter,
            msig      = MethodSignature(
                returnTy          = Type.Void,
                parameterPatterns = List(Pattern.Tuple(
                    mthd.getGenericParameterTypes.toList.zipWithIndex.map(paramPattern)))
            )
        ))
    }
    
    private[this] def methodSymbol(inter: Interval)(mthd: reflect.Method) = {
        val op = {
            if((mthd.getModifiers & reflect.Modifier.STATIC) != 0)
                MethodKind.JavaStatic
            else if((cls.getModifiers & reflect.Modifier.INTERFACE) != 0)
                MethodKind.JavaInterface
            else
                MethodKind.JavaVirtual            
        }
        initReqs(new MethodSymbol(
            pos       = pos,
            modifiers = Modifier.forMember(mthd),
            kind      = MethodKind.Java(
                op,
                cls,
                mthd.getName,
                mthd.getParameterTypes,
                mthd.getReturnType
            ),
            className = name,
            name      = Name.Method(List(mthd.getName)),
            elaborate = inter,
            msig      = MethodSignature(
                returnTy          = typeRef(mthd.getGenericReturnType),
                parameterPatterns = List(Pattern.Tuple(
                    mthd.getGenericParameterTypes.toList.zipWithIndex.map(paramPattern)))
            )
        ))
    }
    
    // Unfortunately, if a class overrides a method with a covariant
    // return type, Java reflection gives us two method symbols!
    // This method purges the entry with the least specific type.
    private[this] def elimCovariantReturns(mthds: Array[reflect.Method]) = {
        val methodMap = new mutable.HashMap[(String, List[Class[_]]), reflect.Method]()
        
        mthds.foreach { mthd =>
            val key = (mthd.getName, mthd.getParameterTypes.toList)
            methodMap.get(key) match {
                case None => 
                    methodMap(key) = mthd
                case Some(prevMthd) =>
                    if(prevMthd.getReturnType.isAssignableFrom(mthd.getReturnType))
                        methodMap(key) = mthd
            }
        }
        
        methodMap.valuesIterator.toList
    }
    
    private[this] def preferNew(
        oldClasses: Array[java.lang.Class[_]],
        newClasses: Array[java.lang.Class[_]]
    ) = {
        // Some(true) if newClass is a boxed version of oldClass
        // Some(false) if oldClass is a boxed version of newClass
        // Otherwise None
        def isPreferable(oldClass: java.lang.Class[_], newClass: java.lang.Class[_]): Option[Boolean] = {
            if(oldClass.isPrimitive && !newClass.isPrimitive) Some(true)
            else if(newClass.isPrimitive && !oldClass.isPrimitive) Some(false)
            else if(oldClass.isArray) {
                assert(newClass.isArray)
                isPreferable(oldClass.getComponentType, newClass.getComponentType)
            }
            else None
        }
        
        // Both of these class signatures mapped to the same Harmonic types.
        // This can happen when primitive types are involved as well as boxed
        // types.  We find the first case where such a difference is found
        // and prefer the boxed version, since it more directly matches what 
        // we have and may (for example) have defined semantics for null.
        (0 until oldClasses.length).firstSome(i => isPreferable(oldClasses(i), newClasses(i))).get
    }
    
    // If there is a java method foo(int) and another foo(Integer), just
    // drop the foo(int) version.  
    private[this] def elimPrimitiveConflicts(msyms: List[MethodSymbol]) = {
        val methodMap = new mutable.HashMap[(Name.Method, List[Pattern.Ref]), MethodSymbol]()
        
        // Safe because we only ever generate symbols with a kind of MethodKind.Java:
        def argClasses(msym: MethodSymbol) =
            msym.kind.asInstanceOf[MethodKind.Java].argumentClasses
        
        msyms.foreach { msym =>
            val key = (msym.name, msym.msig.parameterPatterns)
            methodMap.get(key) match {
                case None => {
                    methodMap(key) = msym                    
                }
                case Some(prevMsym) => {
                    if(preferNew(argClasses(prevMsym), argClasses(msym))) {
                        methodMap(key) = msym
                    }
                }
            }
        }
        
        methodMap.valuesIterator.toList
    }
}

