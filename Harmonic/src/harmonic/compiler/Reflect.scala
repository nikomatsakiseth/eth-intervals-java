package harmonic.compiler

import Ast.{Resolve => out}

import java.lang.reflect

/** Support code for Symbol.ClassFromReflection: 
  * Creates symbols from reflective objects. */
case class Reflect(state: CompilationState) {
    
    def typeArg(pair: (Name.Member, reflect.Type)): Option[Type.TypeArg] = pair match {
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
    
    def typeRef(ty: reflect.Type): Type.Ref = ty match {
        case cls: Class[_] => {
            val name = Name.Class(cls)
            state.requireLoadedOrLoadable(InterPosition.forClass(cls), name)
            Type.Class(name, List())
        }
        case gat: reflect.GenericArrayType => {
            val targ = typeArg(Name.ArrayElem, gat.getGenericComponentType).get
            Type.Class(Name.ArrayClass, List(targ))
        }
        case tv: reflect.TypeVariable[_] => {
            // TODO Method type variables... how can we do it?
            val cls = tv.getGenericDeclaration.asInstanceOf[java.lang.Class[_]]
            val name = Name.Class(cls)
            Type.Var(Path.This, Name.Member(name, tv.getName))
        }
        case pt: reflect.ParameterizedType => {
            val cls = pt.getRawType.asInstanceOf[Class[_]]
            val className = Name.Class(cls)
            val tparams = cls.getTypeParameters.toList.map(tv => Name.Member(className, tv.getName))
            val targs = tparams.zip(pt.getActualTypeArguments).flatMap(typeArg)
            Type.Class(className, targs)
        }
        case _ => throw new RuntimeException("Not here")
    }
    
    def fieldSymbol(csym: Symbol.ClassFromReflection)(fld: reflect.Field) = {
        new VarSymbol.Field(
            modifiers = Modifier.forMember(fld),
            name        = Name.Member(csym.name, fld.getName),
            ty          = typeRef(fld.getGenericType)
        )        
    }
    
    def paramPattern(pair: (reflect.Type, Int)) = {
        Pattern.Var(
            name = Name.LocalVar("arg"+pair._2),
            ty   = typeRef(pair._1)
        )
    }
    
    def superClassNames(sym: Symbol.ClassFromReflection) = {
        val cls = sym.cls
        (cls.getSuperclass :: cls.getInterfaces.toList).filter(_ != null).map(Name.Class)
    }
    
    def fieldSymTabEntry(csym: Symbol.ClassFromReflection)(fld: reflect.Field) = {
        val memberName = Name.Member(csym.name, fld.getName)
        val modifiers = Modifier.forMember(fld)
        if(modifiers.isStatic) SymTab.StaticField(memberName)
        else SymTab.InstanceField(memberName)
    }
    
    def varMembers(csym: Symbol.ClassFromReflection) = {
        csym.optVarMembers.getOrElse {
            val varMembers = csym.cls.getDeclaredFields.map(fieldSymTabEntry(csym)).toList
            csym.optVarMembers = Some(varMembers)
            varMembers
        }
    }
    
    def ctorSymbol(clsName: Name.Class)(mthd: reflect.Constructor[_]) = {
        new MethodSymbol(
            pos         = InterPosition.forClass(mthd.getDeclaringClass),
            modifiers = Modifier.forMember(mthd),
            kind        = Symbol.JavaVirtual,
            clsName     = clsName,
            name        = Name.InitMethod,
            MethodSignature(
                returnTy          = Type.Void,
                receiverTy        = Type.Class(clsName, List()),
                parameterPatterns = List(Pattern.Tuple(
                    mthd.getGenericParameterTypes.toList.zipWithIndex.map(paramPattern)))
            )
        )
    }
    
    def ctors(sym: Symbol.ClassFromReflection) = {
        sym.optCtors.getOrElse {
            val syms = sym.cls.getConstructors.map(ctorSymbol(sym.name)).toList
            sym.optCtors = Some(syms)
            syms
        }
    }
    
    def methodSymbol(clsName: Name.Class)(mthd: reflect.Method) = {
        new MethodSymbol(
            pos         = InterPosition.forClass(mthd.getDeclaringClass),
            modifiers = Modifier.forMember(mthd),
            kind        = Symbol.JavaVirtual, // FIXME 
            clsName     = clsName,
            name        = Name.Method(List(mthd.getName)),
            MethodSignature(
                returnTy = typeRef(mthd.getGenericReturnType),
                receiverTy = Type.Class(clsName, List()),
                parameterPatterns = List(Pattern.Tuple(
                    mthd.getGenericParameterTypes.toList.zipWithIndex.map(paramPattern)))
            )
        )
    }
    
    def methods(csym: Symbol.ClassFromReflection) = {
        csym.optMethods.getOrElse {
            val msyms = csym.cls.getDeclaredMethods.map(methodSymbol(csym.name)).toList
            csym.optMethods = Some(msyms)
            msyms
        }
    }

    def fields(csym: Symbol.ClassFromReflection) = {
        csym.optFields.getOrElse {
            val fsyms = csym.cls.getDeclaredFields.map(fieldSymbol(csym)).toList
            csym.optFields = Some(fsyms)
            fsyms
        }
    }
        
}