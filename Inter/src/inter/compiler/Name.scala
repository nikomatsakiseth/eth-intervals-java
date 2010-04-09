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
    
    val QualRoot = Qual(List())
    
    object Qual {
        def apply(name: String): Qual = new Qual(name.split('.').reverse.toList)
        
        def apply(cls: java.lang.Class[_]): Qual = cls match {
            case _ if cls == classOf[Boolean] => this("java.lang.Boolean")
            case _ if cls == classOf[Char] => this("java.lang.Character")
            case _ if cls == classOf[Byte] => this("java.lang.Byte")
            case _ if cls == classOf[Short] => this("java.lang.Short")
            case _ if cls == classOf[Int] => this("java.lang.Integer")
            case _ if cls == classOf[Long] => this("java.lang.Long")
            case _ if cls == classOf[Float] => this("java.lang.Float")
            case _ if cls == classOf[Double] => this("java.lang.Double")
            case _ if cls == classOf[Unit] => this("java.lang.Void")
            case _ => this(cls.getName)
        }
    }
    
    /** Method names. */
    case class Method(
        parts: List[String]
    ) {
        def javaName = parts.mkString("$")
        
        override def toString = parts.mkString("", "()", "()")
    }
    
    /** Field and variable names. */
    case class Var(
        text: String
    ) {
        def javaName = text
        
        def toPath = PathBase(this)
        
        override def toString = text
    }
    
    abstract class MemberId
    case class MethodId(cls: Qual, method: Method) extends MemberId
    case class FieldId(cls: Qual, method: Method) extends MemberId
    
    sealed abstract class Path
    case class PathBase(v: Var) extends Path
    case class PathField(base: Path, f: Var) extends Path
    
    val ThisVar = Name.Var("this")
    val ThisPath = PathBase(ThisVar)
    
    val MethodVar = Name.Var("method")
    val MethodPath = PathBase(MethodVar)
    
    val VoidQual = Qual("java.lang.Void")

    val ArrayQual = Qual("inter.lang.Array")
    val ArrayElem = Var("E")

    val IntervalTmplQual = Qual("inter.lang.IntervalTmpl")
    val AsyncIntervalTmplQual = Qual("inter.lang.AsyncIntervalTmpl")
    val IntervalTmplParent = Var("P")
    
    object Path {
        def apply(node: Ast.Lower.Path): Name.Path = node match {
            case Ast.Lower.Var(name, _, _) => PathBase(name.name)
            case Ast.Lower.PathField(owner, name, _, _) => PathField(Path(owner), name.name) 
        }
    }
    
}