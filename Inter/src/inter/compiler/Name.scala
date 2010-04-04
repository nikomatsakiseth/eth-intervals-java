package inter.compiler

object Name {
    
    /** Qualified names: used for classes and packages. */
    case class Qual(
        rev_components: List[String]
    ) {
        def components = rev_components.reverse
        def asRelPath = components.mkString("/")
        override def toString = components.mkString(".")

        def /(nm: String) = Name.Qual(nm :: rev_components)
    }
    
    object Qual {
        def apply(name: String) = new Qual(name.split('.').reverse.toList)
    }
    
    /** Method names. */
    case class Method(
        parts: List[String]
    ) {
        override def toString = parts.mkString("", "()", "()")
    }
    
    /** Field and variable names. */
    case class Var(
        text: String
    ) {
        override def toString = text
    }
    
    sealed abstract class Path
    case class PathBase(v: Var) extends Path
    case class PathField(base: Path, f: Var) extends Path
    
    val ThisVar = Name.Var("this")
    val ThisPath = PathBase(ThisVar)
    val ArrayQual = Qual("inter.lang.Array")
    val ArrayElem = Var("E")
    
}