package inter.compiler

import scala.collection.mutable.HashSet
import scala.collection.mutable.Queue

object Type {
    
    // ___ Data Types _______________________________________________________
    
    sealed abstract class Ref
    case class Var(path: Path.Ref, typeVar: Name.Var) extends Ref {
        override def toString = "%s:%s".format(path, typeVar)
    }
    case class Class(name: Name.Qual, typeArgs: List[Type.Arg]) extends Ref {
        override def toString = 
            if(typeArgs.isEmpty) name.toString
            else "%s[%s]".format(name, typeArgs.mkString(", "))
    }
    case class Tuple(typeRefs: List[Type.Ref]) extends Ref {
        override def toString = "(%s)".format(typeRefs.mkString(", "))
    }
    case object Null extends Ref {
        override def toString = "Null"
    }
    
    sealed abstract class Arg
    case class PathArg(name: Name.Var, rel: PcRel, path: Path.Ref) extends Arg {
        override def toString = "%s %s %s".format(name, rel, path)
    }
    case class TypeArg(name: Name.Var, rel: TcRel, ty: Type.Ref) extends Arg {
        override def toString = "%s %s %s".format(name, rel, ty)
    }
    
    val Void = Type.Class(Name.VoidQual, List())    
    
    // ___ Manipulation _____________________________________________________
    
//    def isSubclass(ty_sub: Type.Ref, ty_sup: Type.Ref): Boolean = {
//        (ty_sub, ty_sup) match {
//            case (Type.Var(path_sub, var_sub), Type.Var(path_sup, var_sup)) => false // XXX
//            
//            case (Type.Class(name_sub, args_sub), Type.Class(name_sup, arg_sup)) => false // XXX
//                
//            case (Type.Tuple(tys_sub), Type.Tuple(tys_sup)) => 
//                tys_sub.zip(tys_sup).forall { case (s, t) => isSubclass(s, t) }
//            case (Type.Null, _) => 
//                true
//            case _ => 
//                false
//        }
//    }
//    
//    def matchesByClass(pattern: Symbol.Pattern, ty: Type.Ref): Boolean = {
//        (pattern, ty) match {
//            // Unpack singleton tuples:
//            case (_, Type.Tuple(List(subty))) => matches(pattern, subty)
//            case (Symbol.Tuple(List(subpattern)), _) => matches(subpattern, ty)
//            
//            // Unpack matching tuples:
//            case (Symbol.Tuple(subpatterns), Type.Tuple(subtys)) if sameLength(subpatterns, subtys) =>
//                subpatterns.zip(subtys).forall { case (p, t) => matches(p, t) }
//
//            // Check for singleton tuples:
//            case (Symbol.VarPattern(_, patty), ty) => isSubclass(patty, ty)
//            case _ => false
//        }
//    }
//
}