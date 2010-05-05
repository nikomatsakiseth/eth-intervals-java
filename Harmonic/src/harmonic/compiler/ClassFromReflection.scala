package harmonic.compiler

import java.lang.reflect

class ClassFromReflection(
    name: Name.Class,
    global: Global,
    val cls: java.lang.Class[_]
) extends ClassFromCompiledSource(name, global) {
    
    lazy val modifiers = {
        Modifier.forClass(cls)
    }
    
    lazy val constructors = {
        cls.getConstructors.map(ctorSymbol).toList
    }
        
    lazy val varMembers = {
        cls.getDeclaredFields.map(fieldSymTabEntry).toList
    }
        
    lazy val allMethodSymbols = {
        cls.getDeclaredMethods.map(methodSymbol).toList
    }
        
    lazy val fields = {
        cls.getDeclaredFields.map(fieldSymbol).toList
    }        
    
    lazy val superClassNames = {
        val allNames = (cls.getSuperclass :: cls.getInterfaces.toList).filter(_ != null).map(Name.Class)
        allNames.foreach(global.requireLoadedOrLoadable(pos, _))
        allNames
    }
    
    def methodsNamed(name: Name.Method) = {
        allMethodSymbols.filter(_.isNamed(name))        
    }
    
    def fieldNamed(name: Name.Member) = {
        fields.find(_.isNamed(name))        
    }
    
    private[this] def fieldSymTabEntry(fld: reflect.Field) = {
        val memberName = Name.Member(name, fld.getName)
        val modifiers = Modifier.forMember(fld)
        if(modifiers.isStatic) SymTab.StaticField(memberName)
        else SymTab.InstanceField(memberName)
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
    
    private[this] def typeRef(ty: reflect.Type): Type.Ref = ty match {
        case ty: Class[_] => {
            val name = Name.Class(ty)
            global.requireLoadedOrLoadable(InterPosition.forClass(ty), name)
            Type.Class(name, List())
        }
        case ty: reflect.GenericArrayType => {
            val targ = typeArg(Name.ArrayElem, ty.getGenericComponentType).get
            Type.Class(Name.ArrayClass, List(targ))
        }
        case ty: reflect.TypeVariable[_] => {
            // TODO Method type variables... how can we do it?
            val cls = ty.getGenericDeclaration.asInstanceOf[java.lang.Class[_]]
            val name = Name.Class(cls)
            Type.Var(Path.This, Name.Member(name, ty.getName))
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
            modifiers = Modifier.forMember(fld),
            name      = Name.Member(name, fld.getName),
            ty        = typeRef(fld.getGenericType)
        )        
    }
    
    private[this] def paramPattern(pair: (reflect.Type, Int)) = {
        Pattern.Var(
            name = Name.LocalVar("arg"+pair._2),
            ty   = typeRef(pair._1)
        )
    }
    
    private[this] def ctorSymbol(mthd: reflect.Constructor[_]) = {
        new MethodSymbol(
            pos       = InterPosition.forClass(mthd.getDeclaringClass),
            modifiers = Modifier.forMember(mthd),
            kind      = MethodKind.JavaVirtual,
            clsName   = name,
            name      = Name.InitMethod,
            MethodSignature(
                returnTy          = Type.Void,
                receiverTy        = toType,
                parameterPatterns = List(Pattern.Tuple(
                    mthd.getGenericParameterTypes.toList.zipWithIndex.map(paramPattern)))
            )
        )
    }
    
    private[this] def methodSymbol(mthd: reflect.Method) = {
        new MethodSymbol(
            pos       = InterPosition.forClass(mthd.getDeclaringClass),
            modifiers = Modifier.forMember(mthd),
            kind      = MethodKind.JavaVirtual, // FIXME 
            clsName   = name,
            name      = Name.Method(List(mthd.getName)),
            MethodSignature(
                returnTy          = typeRef(mthd.getGenericReturnType),
                receiverTy        = toType,
                parameterPatterns = List(Pattern.Tuple(
                    mthd.getGenericParameterTypes.toList.zipWithIndex.map(paramPattern)))
            )
        )
    }
}

