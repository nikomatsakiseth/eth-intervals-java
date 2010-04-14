package inter.compiler

import Ast.{Resolve => out}

import java.lang.reflect

/** Support code for Symbol.ClassFromReflection: 
  * Creates symbols from reflective objects. */
object Reflect {
    
    def typeArg(pair: (Name.Var, reflect.Type)): Option[Type.TypeArg] = pair match {
        case (nm, wt: reflect.WildcardType) => { /* XXX: Allow multiple LB, UB? */
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
        case cls: Class[_] => Type.Class(Name.Qual(cls), List())
        case gat: reflect.GenericArrayType => {
            val targ = typeArg(Name.ArrayElem, gat.getGenericComponentType).get
            Type.Class(Name.ArrayQual, List(targ))
        }
        case tv: reflect.TypeVariable[_] => Type.Var(Path.This, Name.Var(tv.getName))
        case pt: reflect.ParameterizedType => {
            val cls = pt.getRawType.asInstanceOf[Class[_]]
            val tparams = cls.getTypeParameters.toList.map(tv => Name.Var(tv.getName))
            val targs = tparams.zip(pt.getActualTypeArguments).flatMap(typeArg)
            Type.Class(Name.Qual(cls), targs)
        }
        case _ => throw new RuntimeException("Not here")
    }
    
    def fieldSymbol(state: CompilationState)(fld: reflect.Field) = new Symbol.Var(
        name = Name.Var(fld.getName),
        ty = typeRef(fld.getGenericType)
    )
    
    def paramPattern(pair: (reflect.Type, Int)) = Pattern.Var(
        name = Name.Var("arg"+pair._2),
        ty = typeRef(pair._1)
    )
    
    def methodSymbol(state: CompilationState, clsName: Name.Qual)(mthd: reflect.Method) = new Symbol.Method(
        name = Name.Method(List(mthd.getName)),
        returnTy = typeRef(mthd.getGenericReturnType),
        receiver = Pattern.Var(Name.ThisVar, Type.Class(clsName, List())),
        parameterPatterns = List(Pattern.Tuple(
            mthd.getGenericParameterTypes.toList.zipWithIndex.map(paramPattern)))
    )
    
    def methodsNamed(state: CompilationState, sym: Symbol.ClassFromReflection, name: Name.Method) = {
        val methods = sym.optMethods.getOrElse {
            val syms = sym.cls.getDeclaredMethods.map(Reflect.methodSymbol(state, sym.name)).toList
            sym.optMethods = Some(syms)
            syms
        }
        methods.filter(_.name == name)        
    }

    def fieldNamed(state: CompilationState, sym: Symbol.ClassFromReflection, name: Name.Var) = {
        val fields = sym.optFields.getOrElse {
            val syms = sym.cls.getDeclaredFields.map(Reflect.fieldSymbol(state)).toList
            sym.optFields = Some(syms)
            syms
        }
        fields.find(_.name == name)        
    }
        
}