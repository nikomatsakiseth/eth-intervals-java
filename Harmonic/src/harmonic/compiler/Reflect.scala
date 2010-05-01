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
            Type.Var(Path.This, Name.Member(tv.getName))
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
    
    def fieldSymbol(fld: reflect.Field) = {
        new Symbol.Var(
            modifierSet = Modifier.forMember(fld),
            name        = Name.Var(fld.getName),
            ty          = typeRef(fld.getGenericType)
        )        
    }
    
    def paramPattern(pair: (reflect.Type, Int)) = {
        Pattern.Var(
            name = Name.Var("arg"+pair._2),
            ty   = typeRef(pair._1)
        )
    }
    
    def superClassNames(sym: Symbol.ClassFromReflection) = {
        val cls = sym.cls
        (cls.getSuperclass :: cls.getInterfaces.toList).flatMap(Option(_)).map(Name.Class)
    }
    
    def ctorSymbol(clsName: Name.Qual)(mthd: reflect.Constructor[_]) = {
        new Symbol.Method(
            pos         = InterPosition.forClass(mthd.getDeclaringClass),
            modifierSet = Modifier.forMember(mthd),
            kind        = Symbol.JavaVirtual,
            clsName     = clsName,
            name        = Name.InitMethod,
            Symbol.MethodSignature(
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
    
    def methodSymbol(clsName: Name.Qual)(mthd: reflect.Method) = {
        new Symbol.Method(
            pos         = InterPosition.forClass(mthd.getDeclaringClass),
            modifierSet = Modifier.forMember(mthd),
            kind        = Symbol.JavaVirtual, // FIXME 
            clsName     = clsName,
            name        = Name.Method(List(mthd.getName)),
            Symbol.MethodSignature(
                returnTy = typeRef(mthd.getGenericReturnType),
                receiverTy = Type.Class(clsName, List()),
                parameterPatterns = List(Pattern.Tuple(
                    mthd.getGenericParameterTypes.toList.zipWithIndex.map(paramPattern)))
            )
        )
    }
    
    def methods(sym: Symbol.ClassFromReflection) = {
        sym.optMethods.getOrElse {
            val syms = sym.cls.getDeclaredMethods.map(methodSymbol(sym.name)).toList
            sym.optMethods = Some(syms)
            syms
        }
    }

    def fields(sym: Symbol.ClassFromReflection) = {
        sym.optFields.getOrElse {
            val syms = sym.cls.getDeclaredFields.map(fieldSymbol).toList
            sym.optFields = Some(syms)
            syms
        }
    }
        
}