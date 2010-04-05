package inter.compiler

import Hl.{RN => out}

import java.lang.reflect.Field
import java.lang.reflect.Method
import java.lang.reflect.Type
import java.lang.reflect.GenericArrayType
import java.lang.reflect.TypeVariable
import java.lang.reflect.ParameterizedType
import java.lang.reflect.WildcardType

object ReflectPass {
    
    def qualName(cls: Class[_]) = {
        import Name.{Qual => qn}
        cls match {
            case _ if cls == classOf[Boolean] => qn("java.lang.Boolean")
            case _ if cls == classOf[Char] => qn("java.lang.Character")
            case _ if cls == classOf[Byte] => qn("java.lang.Byte")
            case _ if cls == classOf[Short] => qn("java.lang.Short")
            case _ if cls == classOf[Int] => qn("java.lang.Integer")
            case _ if cls == classOf[Long] => qn("java.lang.Long")
            case _ if cls == classOf[Float] => qn("java.lang.Float")
            case _ if cls == classOf[Double] => qn("java.lang.Double")
            case _ if cls == classOf[Unit] => qn("java.lang.Void")
            case _ => qn(cls.getName)
        }
    }
    
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
        case cls: Class[_] => Symbol.ClassType(qualName(cls), List())
        case gat: GenericArrayType => {
            val targ = typeArg(Name.ArrayElem, gat.getGenericComponentType).get
            Symbol.ClassType(Name.ArrayQual, List(targ))
        }
        case tv: TypeVariable[_] => Symbol.PathType(Name.ThisPath, Name.Var(tv.getName))
        case pt: ParameterizedType => {
            val cls = pt.getRawType.asInstanceOf[Class[_]]
            val tparams = cls.getTypeParameters.toList.map(tv => Name.Var(tv.getName))
            val targs = tparams.zip(pt.getActualTypeArguments).flatMap(typeArg)
            Symbol.ClassType(qualName(cls), targs)
        }
        case _ => throw new RuntimeException("Not here")
    }
    
    def fieldSymbol(fld: Field) = new Symbol.Var(
        name = Name.Var(fld.getName),
        typeRef = typeRef(fld.getGenericType)
    )
    
    def paramSymbol(pair: (Type, Int)) = new Symbol.Var(
        name = Name.Var("arg"+pair._2),
        typeRef = typeRef(pair._1)
    )
    
    def methodSymbol(mthd: Method) = new Symbol.Method(
        name = Name.Method(List(mthd.getName)),
        retTypeRef = typeRef(mthd.getGenericReturnType),
        parameters = mthd.getGenericParameterTypes.toList.zipWithIndex.map(paramSymbol)
    )
    
    def classSymbol(cls: Class[_]) = {
        val clssym = new Symbol.Class(qualName(cls))
        clssym.fields ++= cls.getDeclaredFields.map(fieldSymbol)
        clssym.methods ++= cls.getDeclaredMethods.map(methodSymbol)
        clssym
    }
    
    def apply(state: CompilationState, cls: Class[_]) {
        val sym = classSymbol(cls)
        state.symtab.classes(sym.name) = sym
    }
    
}