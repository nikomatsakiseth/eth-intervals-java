package harmonic.compiler

import scala.collection.immutable.Set

import Util._

object Path {
    
    sealed abstract trait Owner
    case object Static extends Owner
    
    // ___ Untyped Paths ____________________________________________________
    
    sealed abstract class Ref extends Owner {
        def is(r: Ref) = (this == r)
        def /(fname: Name.Member) = Field(this, fname)
        def call(methodId: MethodId, args: Path.Ref*) = Call(this, methodId, args.toList)
        def unapply(r: Ref) = (this == r)
    }
    case class Local(v: Name.LocalVar) extends Ref {
        override def toString = v.toString
    }
    case class Cast(ty: Type, path: Ref) extends Ref {
        override def toString = "(%s)%s".format(ty, path)
    }
    case class Constant(obj: Object) extends Ref {
        override def toString = obj match {
            case obj: String => '"' + obj.toString + '"'
            case _ => obj.toString
        }
    }
    case class Field(base: Owner, f: Name.Member) extends Ref {
        override def toString = base.toString + "." + f.toString
    }
    case class Call(receiver: Owner, methodId: MethodId, args: List[Path.Ref]) extends Ref {
        override def toString = "%s.%s(%s)".format(receiver, methodId, args.mkString(", "))
    }
    case class Index(array: Path.Ref, index: Path.Ref) extends Ref {
        override def toString = "%s[%s]".format(array, index)
    }
    case class Tuple(paths: List[Ref]) extends Ref {
        override def toString = "(%s)".format(paths.mkString(","))
    }
    
    object Constant {
        def integer(idx: Int) = Constant(java.lang.Integer.valueOf(idx))
    }
    
    val This = Name.ThisLocal.toPath
    val Method = Name.MethodLocal.toPath
    val Final = Field(Static, Name.FinalMember)
    val RacyGuard = Field(Static, Name.RacyMember)
    val ThisInit = This / Name.Init
    val ThisWr = This / Name.Wr

}