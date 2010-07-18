package harmonic.compiler

import scala.collection.immutable.Set

import Util._

sealed trait Path extends Path.Owner {
    def is(r: Path) = (this == r)
    def /(fname: Name.Member) = Path.Field(this, fname)
    def call(methodId: MethodId, args: Path*) = Path.Call(this, methodId, args.toList)
    def unapply(r: Path) = (this == r)
}

object Path {
    
    sealed abstract trait Owner
    
    case object Static extends Owner
    
    case class Local(v: Name.LocalVar) extends Path {
        override def toString = v.toString
    }
    
    case class Cast(ty: Type, path: Path) extends Path {
        override def toString = "(%s)%s".format(ty, path)
    }
    
    case class Constant(obj: Object) extends Path {
        override def toString = obj match {
            case obj: String => '"' + obj.toString + '"'
            case _ => obj.toString
        }
    }
    
    case class Field(base: Owner, f: Name.Member) extends Path {
        override def toString = base.toString + "." + f.toString
    }
    
    case class Call(receiver: Owner, methodId: MethodId, args: List[Path]) extends Path {
        override def toString = "%s.%s(%s)".format(receiver, methodId, args.mkString(", "))
    }
    
    case class Index(array: Path, index: Path) extends Path {
        override def toString = "%s[%s]".format(array, index)
    }
    
    case class Tuple(paths: List[Path]) extends Path {
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