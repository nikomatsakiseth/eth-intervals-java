package harmonic.compiler

import ch.ethz.intervals._

import java.lang.reflect
import java.lang.reflect.{Modifier => jModifier}

import scala.collection.mutable

import com.smallcultfollowing.lathos.Lathos

import harmonic.jcompat.Ghosts
import harmonic.jcompat.Ghost

import harmonic.lang.Mutable
import harmonic.lang.Requires

import Intrinsic.toHarmonicAnnotated

import Util._

class ClassFromReflection(
    val name: Name.Class,
    val global: Global,
    val cls: java.lang.Class[_]
) extends ClassFromCompiledSource {
    
    def isObject = name is Name.ObjectClass
    def isJavaInterface = cls.getModifiers.containsBits(jModifier.INTERFACE)
    def isPrivate(member: reflect.Member) = member.getModifiers.containsBits(jModifier.PRIVATE)
    def isDeclaredByCls(member: reflect.Member) = (member.getDeclaringClass == cls)
    def isSuitable(member: reflect.Member) = isDeclaredByCls(member) && !isPrivate(member)
    
    override def toReflectedJavaClass = Some(cls)
    
    protected[this] def loadData(inter: Interval) = Data(
        modifiers = Modifier.forClass(cls),
        
        superClassNames = {
            val superClasses = {
                val interfaceClasses = cls.getInterfaces.toList
                val classesWithSuper = Option(cls.getSuperclass) ?:: interfaceClasses
                classesWithSuper match {
                    case Nil if isObject => Nil
                    case Nil => List(classOf[Object])
                    case list => list
                }
            }
            val superNames = for(c <- superClasses) yield Name.Class(c)
            superNames.foreach(global.requireLoadedOrLoadable(pos, _))
            superNames
        },
        
        superTypes = {
            // Note: does not include java.lang.Object if this is an interface, enum, etc
            val reflTypes = {
                val interfaceTypes = cls.getGenericInterfaces.toList
                Option(cls.getGenericSuperclass) ?:: interfaceTypes
            }
            val tys = for(rt <- reflTypes) yield typeRef(rt).asInstanceOf[Type.Class]
            tys match {
                case Nil if isObject => Nil
                case Nil => List(Type.Object)
                case list => list
            }
        },
        
        varMembers = List[Array[SymTab.Entry]](
            Intrinsic(global).extraVarMembers(name),
            ghostSymTabEntries(cls.getHAnnotation(classOf[Ghosts])),
            cls.getDeclaredFields.map(fieldSymTabEntry),
            cls.getTypeParameters.map(typeParamSymTabEntry)
        ).flatten,
        
        constructors = {
            val ctors = cls.getConstructors
            if(!ctors.isEmpty) {
                ctors.map(ctorSymbol(inter)).toList            
            } else { // gin up an empty constructor for interfaces:
                List(
                    initReqs(None)(
                        new MethodSymbol(
                            pos       = pos, 
                            modifiers = Modifier.Set.empty,
                            kind      = MethodKind.JavaDummyCtor,
                            className = name,
                            name      = Name.InitMethod,
                            elaborate = inter,
                            msig      = MethodSignature(Type.Void, List(Pattern.EmptyTuple))
                        )
                    )
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
            cls.getDeclaredFields.filter(isSuitable).map(fieldSymbol(inter))
        ).flatten,
        
        allIntervalSymbols = Nil,
        
        allGhostSymbols = List(
            Intrinsic(global).extraGhostSymbols(name),
            ghostSymbols(cls.getHAnnotation(classOf[Ghosts]))
        ).flatten,
        
        checkEnv = Env.empty(global)
    )
    
    private[this] def initReqs(robj: Option[reflect.AccessibleObject])(msym: MethodSymbol) = {
        // TODO Honor @Requires and @Ensures annotations
        msym.Requirements.v = Nil
        msym.Ensures.v = Nil
        msym.GuardPath.v = robj.flatMap(_.getHAnnotation(classOf[Mutable])) match {
            case None => 
                Path.RacyGuard
                
            case Some(path) => 
                AnnParse.parsePath(path.value) match {
                    case Left(err) => {
                        err.report(global, pos)
                        Path.RacyGuard
                    }
                
                    case Right(path) => {
                        path
                    }
                }
        }
        
        msym
    }
    
    private[this] def fieldSymTabEntry(fld: reflect.Field) = {
        val memberName = Name.Member(name, fld.getName)
        val modifiers = Modifier.forMember(fld)
        if(modifiers.isStatic) SymTab.StaticField(memberName)
        else SymTab.InstanceField(memberName)
    }
    
    private[this] def ghostSymTabEntries(optGhosts: Option[Ghosts]): Array[SymTab.Entry] = {
        optGhosts match {
            case None => 
                Array()
            case Some(ghosts) =>
                for(g <- ghosts.value) yield SymTab.Ghost(Name.Member(name, g.name))
        }
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
    
    private[this] def fieldSymbol(inter: Interval)(fld: reflect.Field) = {
        val fsym = new VarSymbol.Field(
            pos       = pos,
            modifiers = Modifier.forMember(fld),
            name      = Name.Member(name, fld.getName),
            ty        = typeRef(fld.getGenericType),
            kind      = FieldKind.Java(cls, fld.getName, fld.getType),
            elaborate = inter
        )
        
        fsym.GuardPath.v = fld.getHAnnotation(classOf[Mutable]) match {
            case None if fld.getModifiers.containsBits(jModifier.FINAL) => 
                Path.Final
            case None if fld.getModifiers.containsBits(jModifier.STATIC) => 
                Path.RacyGuard
            case None => 
                Path.ThisWr
            case Some(path) => 
                AnnParse.parsePath(path.value) match {
                    case Left(err) => {
                        err.report(global, pos)
                        Path.ThisWr
                    }
                
                    case Right(path) => {
                        path
                    }
                }
        }
        
        fsym
    }
    
    private[this] def ghostSymbol(ghost: Ghost) = {
        new GhostSymbol(
            pos,
            Name.Member(name, ghost.name),
            Name.Class(ghost.bound)
        )
    }
    
    private[this] def ghostSymbols(optGhosts: Option[Ghosts]): Array[GhostSymbol] = {
        optGhosts match {
            case None => 
                Array()
            case Some(ghosts) =>
                for(g <- ghosts.value) yield ghostSymbol(g)
        }
    }
    
    private[this] def paramPattern(pair: (reflect.Type, Int)) = {
        Pattern.Var(
            name = Name.LocalVar("arg"+pair._2),
            ty   = typeRef(pair._1)
        )
    }
    
    private[this] def ctorSymbol(inter: Interval)(mthd: reflect.Constructor[_]) = {
        initReqs(Some(mthd))(
            new MethodSymbol(
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
            )
        )
    }
    
    private[this] def methodSymbol(inter: Interval)(mthd: reflect.Method) = {
        val op = {
            if(mthd.getModifiers.containsBits(jModifier.STATIC))
                MethodKind.JavaStatic
            else if(cls.getModifiers.containsBits(jModifier.INTERFACE))
                MethodKind.JavaInterface
            else
                MethodKind.JavaVirtual            
        }
        initReqs(Some(mthd))(
            new MethodSymbol(
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
            )
        )
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

