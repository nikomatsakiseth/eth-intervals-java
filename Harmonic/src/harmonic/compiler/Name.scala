package harmonic.compiler

object Name {
    
    /** Qualified names: used for classes and packages. */
    case class Qual(
        rev_components: List[String]
    ) {
        def components = rev_components.reverse
        def asRelPath = components.mkString("/")
        override def toString = components.mkString(".")
        
        def internalName = asRelPath
        
        def /(nm: String) = Name.Qual(nm :: rev_components)
        
        def withSuffix(suffix: String) = rev_components match {
            case hd :: tl => Name.Qual((hd + suffix) :: tl)
            case List() => throw new RuntimeException("Non-empty name expected")
        }
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
        
        def toPath = Path.Base(this)
        
        override def toString = text
    }
    
    val ThisVar = Name.Var("this")
    
    val MethodVar = Name.Var("method")
    
    val VoidQual = Qual(classOf[java.lang.Void])
    
    val ObjectQual = Qual(classOf[java.lang.Object])

    val ArrayQual = Qual("inter.lang.Array")
    val ArrayElem = Var("E")

    val BlockQual = Qual(classOf[harmonic.lang.Block[_, _]])
    val AsyncBlockQual = Qual(classOf[harmonic.lang.AsyncBlock[_, _]])
    val RVar = Name.Var("R")
    val AVar = Name.Var("A")
    val IntervalTmplParent = Var("Parent")
    val ValueMethod = Method(List("value"))

    val InitMethod = Method(List("<init>"))
       
}