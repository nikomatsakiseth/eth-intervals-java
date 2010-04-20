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
    object Class {
        def apply(cls: java.lang.Class[_]): Type.Class = Class(Name.Qual(cls), List())
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

}