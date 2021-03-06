package harmonic.compiler

import scala.collection.mutable.HashSet
import scala.collection.mutable.Queue

sealed abstract class Type {
    def is(ty: Type) = (this == ty)
    def unapply(ty: Type) = (this == ty)    
}

object Type {
    
    case class Member(path: Path, typeVar: Name.Member) extends Type {
        override def toString = "%s.%s".format(path, typeVar)
    }
    case class Class(name: Name.Class, typeArgs: List[Type.Arg]) extends Type {
        override def toString = 
            if(typeArgs.isEmpty) name.toString
            else "%s[%s]".format(name, typeArgs.mkString(", "))
    }
    case class Tuple(typeRefs: List[Type]) extends Type {
        override def toString = "(%s)".format(typeRefs.mkString(", "))
    }
    case object Null extends Type {
        override def toString = "Null"
    }
    
    sealed abstract class Arg {
        def name: Name.Member
    }
    case class PathArg(name: Name.Member, rel: PcRel, path: Path) extends Arg {
        override def toString = "%s %s %s".format(name, rel, path)
    }
    case class TypeArg(name: Name.Member, rel: TcRel, ty: Type) extends Arg {
        override def toString = "%s %s %s".format(name, rel, ty)
    }
    
    object Class {
        def apply(cls: java.lang.Class[_]): Type.Class = Class(Name.Class(cls), List())
    }
    
    val Object = Type.Class(Name.ObjectClass, List())
    val Void = Type.Class(Name.VoidClass, List())
    val RoInterval = Type.Class(Name.RoIntervalClass, List())
    val Interval = Type.Class(Name.IntervalClass, List())
    val RoPoint = Type.Class(Name.RoPointClass, List())
    val Point = Type.Class(Name.PointClass, List())
    val AsyncInterval = Type.Class(Name.AsyncIntervalClass, List())
    val InlineInterval = Type.Class(Name.InlineIntervalClass, List())
    val Guard = Type.Class(Name.GuardClass, List())

    // Use Type.Top when you want the most general
    // type you can get, of which all others are a subtype.
    val Top = Object

    // Returns `Array[E <: ty]`
    def arrayExtends(ty: Type) = {
        Class(
            Name.ArrayClass,
            List(
                TypeArg(Name.ArrayElem, TcSub, ty)
            )
        )
    }
    
}