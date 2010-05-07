package harmonic.compiler

import scala.collection.mutable.HashSet
import scala.collection.mutable.Queue

object Type {
    
    sealed abstract class Ref
    case class Var(path: Path.Ref, typeVar: Name.Member) extends Ref {
        override def toString = "%s.%s".format(path, typeVar)
    }
    case class Class(name: Name.Class, typeArgs: List[Type.Arg]) extends Ref {
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
    case class PathArg(name: Name.Member, rel: PcRel, path: Path.Ref) extends Arg {
        override def toString = "%s %s %s".format(name, rel, path)
    }
    case class TypeArg(name: Name.Member, rel: TcRel, ty: Type.Ref) extends Arg {
        override def toString = "%s %s %s".format(name, rel, ty)
    }
    
    object Class {
        def apply(cls: java.lang.Class[_]): Type.Class = Class(Name.Class(cls), List())
    }
    
    val Object = Type.Class(Name.ObjectClass, List())
    val Void = Type.Class(Name.VoidClass, List())
    val Interval = Type.Class(Name.IntervalClass, List())
    
}