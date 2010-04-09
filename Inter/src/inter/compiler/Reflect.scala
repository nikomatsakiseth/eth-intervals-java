package inter.compiler

import Ast.{Resolve => out}

import java.lang.reflect.Field
import java.lang.reflect.Method
import java.lang.reflect.Type
import java.lang.reflect.GenericArrayType
import java.lang.reflect.TypeVariable
import java.lang.reflect.ParameterizedType
import java.lang.reflect.WildcardType

/** Support code for Symbol.ClassFromReflection: 
  * Creates symbols from reflective objects. */
object Reflect {
    
    def typeArg(pair: (Name.Var, Type)): Option[Symbol.TypeArg] = pair match {
        case (nm, wt: WildcardType) => { /* XXX: Allow multiple LB, UB? */
            val lbs = wt.getLowerBounds
            val ubs = wt.getUpperBounds
            if(!lbs.isEmpty) {
                Some(Symbol.TypeTypeArg(nm, TcSup, typeRef(lbs(0))))
            } else if(!ubs.isEmpty) {
                Some(Symbol.TypeTypeArg(nm, TcSub, typeRef(ubs(0))))
            } else None
        }
        case (nm, ty) => Some(Symbol.TypeTypeArg(nm, TcEq, typeRef(ty)))
    }
    
    def typeRef(ty: Type): Symbol.Type = ty match {
        case cls: Class[_] => Symbol.ClassType(Name.Qual(cls), List())
        case gat: GenericArrayType => {
            val targ = typeArg(Name.ArrayElem, gat.getGenericComponentType).get
            Symbol.ClassType(Name.ArrayQual, List(targ))
        }
        case tv: TypeVariable[_] => Symbol.PathType(Name.ThisPath, Name.Var(tv.getName))
        case pt: ParameterizedType => {
            val cls = pt.getRawType.asInstanceOf[Class[_]]
            val tparams = cls.getTypeParameters.toList.map(tv => Name.Var(tv.getName))
            val targs = tparams.zip(pt.getActualTypeArguments).flatMap(typeArg)
            Symbol.ClassType(Name.Qual(cls), targs)
        }
        case _ => throw new RuntimeException("Not here")
    }
    
    def fieldSymbol(state: CompilationState)(fld: Field) = new Symbol.Var(
        name = Name.Var(fld.getName),
        ty = typeRef(fld.getGenericType)
    )
    
    def paramPattern(pair: (Type, Int)) = Symbol.VarPattern(
        name = Name.Var("arg"+pair._2),
        ty = typeRef(pair._1)
    )
    
    def methodSymbol(state: CompilationState, clsName: Name.Qual)(mthd: Method) = new Symbol.Method(
        name = Name.Method(List(mthd.getName)),
        returnTy = typeRef(mthd.getGenericReturnType),
        receiver = Symbol.VarPattern(Name.ThisVar, Symbol.ClassType(clsName, List())),
        parameterPatterns = List(Symbol.TuplePattern(
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