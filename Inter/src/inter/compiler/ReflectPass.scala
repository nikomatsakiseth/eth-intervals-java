package inter.compiler

import Hl.{RN => out}

import java.lang.reflect.Field
import java.lang.reflect.Method
import java.lang.reflect.Type
import java.lang.reflect.GenericArrayType
import java.lang.reflect.TypeVariable
import java.lang.reflect.ParameterizedType
import java.lang.reflect.WildcardType

class ReflectPass(val state: CompilationState) {
    import Hl.abs
    import state.symtab
    
    def absName(clss: Array[Class[_]]) = {
        clss.toList.map(Option(_)).flatMap {
            case None => None
            case Some(cls) if cls == classOf[Boolean] => Some(abs("java.lang.Boolean"))
            case Some(cls) if cls == classOf[Char] => Some(abs("java.lang.Character"))
            case Some(cls) if cls == classOf[Byte] => Some(abs("java.lang.Byte"))
            case Some(cls) if cls == classOf[Short] => Some(abs("java.lang.Short"))
            case Some(cls) if cls == classOf[Int] => Some(abs("java.lang.Integer"))
            case Some(cls) if cls == classOf[Long] => Some(abs("java.lang.Long"))
            case Some(cls) if cls == classOf[Float] => Some(abs("java.lang.Float"))
            case Some(cls) if cls == classOf[Double] => Some(abs("java.lang.Double"))
            case Some(cls) if cls == classOf[Unit] => Some(abs("java.lang.Void"))
            case Some(cls) => Some(abs(cls.getName))
        }
    }
    
    def typeArg(pair: (Name.Var, Type)): Option[symtab.TypeArg] = pair match {
        case (nm, wt: WildcardType) => { /* XXX: Allow multiple LB, UB? */
            val lbs = wt.getLowerBounds
            val ubs = wt.getUpperBounds
            if(!lbs.isEmpty) {
                Some(symtab.TypeTypeArg(nm, TcSup, typeRef(lbs(0))))
            } else if(!ubs.isEmpty) {
                Some(symtab.TypeTypeArg(nm, TcSub, typeRef(ubs(0))))
            } else None
        }
        case (nm, ty) => Some(symtab.TypeTypeArg(nm, TcEq, typeRef(ty)))
    }
    
    def typeRef(ty: Type): symtab.Type = ty match {
        case cls: Class[_] => symtab.ClassType(Name.Qual(cls.getName), List())
        case gat: GenericArrayType => {
            val targ = typeArg(Name.ArrayElem, gat.getGenericComponentType).get
            symtab.ClassType(Name.ArrayQual, List(targ))
        }
        case tv: TypeVariable[_] => symtab.PathType(Name.ThisPath, Name.Var(tv.getName))
        case pt: ParameterizedType => {
            val cls = pt.getRawType.asInstanceOf[Class[_]]
            val tparams = cls.getTypeParameters.toList.map(tv => Name.Var(tv.getName))
            val targs = tparams.zip(pt.getActualTypeArguments).flatMap(typeArg)
            symtab.ClassType(Name.Qual(cls.getName), targs)
        }
        case _ => throw new RuntimeException("Not here")
    }
    
    def fieldSymbol(fld: Field) = new state.symtab.VarSymbol(
        name = Name.Var(fld.getName),
        typeRef = typeRef(fld.getGenericType)
    )
    
    def methodSymbol(mthd: Method) = new state.symtab.MethodSymbol(
        name = Name.Method(List(mthd.getName)),
        retTypeRef = typeRef(mthd.getGenericReturnType),
        parameters = mthd.getGenericParameterTypes.toList.map(typeRef)
    )
    
    def classSymbol(cls: Class[_]) = {
        val clssym = new state.symtab.ClassSymbol(Name.Qual(cls.getName))
        clssym.fields ++= cls.getDeclaredFields.map(fieldSymbol)
        clssym.methods ++= cls.getDeclaredMethods.map(methodSymbol)
        clssym
    }
    
}

object ReflectPass {
    
    def apply(state: CompilationState, cls: Class[_]) {
        new ReflectPass(state).classSymbol(cls)
    }
    
}