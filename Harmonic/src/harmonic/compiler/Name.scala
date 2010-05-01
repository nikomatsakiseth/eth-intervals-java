package harmonic.compiler

object Name {
    
    sealed abstract class Qual {
        def toInternalPrefix: String
        def toPrefix: String
        def asClassName: Option[Class]
    }
    
    sealed abstract class Package extends Qual {
        def asClassName = None
    }
    
    case object Root extends Package {
        def toInternalPrefix = ""
        def toPrefix = ""
        override def toString = "<root>"
    }
    
    case class Subpackage(
        base: Package,
        name: String
    ) extends Package {
        def toInternalPrefix = base.toInternalPrefix + name + "/"
        def toPrefix = base.toPrefix + name + "."
        override def toString = base.toPrefix + name
    }
    
    case class Class(
        base: Qual,
        name: String
    ) extends Qual {
        def relPath: String = internalName
        def internalName = base.toInternalPrefix + name
        def toInternalPrefix = internalName + "$"
        def toPrefix = base.toPrefix + name + "."
        override def toString = base.toPrefix + name
        def withSuffix(suffix: String) = Class(base, name + suffix)
        def asClassName = Some(this)
    }
    
    object Package {
        
        def apply(str: String): Package = {
            val comp = str.split(".")
            comp.foldLeft[Package](Root) { case (q, n) => Subpackage(q, n) }
        }
        
        def apply(pkg: java.lang.Package): Package = {
            if(pkg == null) Root
            else apply(pkg.getName)
        }
        
    }
    
    object Class extends (java.lang.Class[_] => Class) {
        def apply(cls: java.lang.Class[_]): Class = cls match {
            // Handle the primitives:
            case _ if cls == classOf[Boolean] => apply(classOf[java.lang.Boolean])
            case _ if cls == classOf[Char] => apply(classOf[java.lang.Character])
            case _ if cls == classOf[Byte] => apply(classOf[java.lang.Byte])
            case _ if cls == classOf[Short] => apply(classOf[java.lang.Short])
            case _ if cls == classOf[Int] => apply(classOf[java.lang.Integer])
            case _ if cls == classOf[Long] => apply(classOf[java.lang.Long])
            case _ if cls == classOf[Float] => apply(classOf[java.lang.Float])
            case _ if cls == classOf[Double] => apply(classOf[java.lang.Double])
            case _ if cls == classOf[Unit] => apply(classOf[java.lang.Void])
            
            // Handle inner classes:
            case _ if cls.getDeclaringClass != null => {
                Class(apply(cls.getDeclaringClass), cls.getSimpleName)
            }
            
            // Handle top-level classes:
            case _ => {
                Class(Package(cls.getPackage), cls.getSimpleName)
            }
        }
    }
    
    /** Method names. */
    case class Method(
        parts: List[String]
    ) {
        def javaName = parts.mkString("$")
        
        override def toString = parts.mkString("", "()", "()")
    }
    
    /** Names of variables */
    sealed abstract class Var {
        def javaName: String
        
        // The shorthand form the user would use to refer
        // to this variable.  For example, a field "foo"
        // in a class "Bar" would return "foo".
        def text: String
        
        def toPath = Path.Base(this)
    }
    
    /** Fully specified names of members (except for methods) */
    sealed case class Member(
        className: Name.Class,
        text: String
    ) extends Var with UnloweredMember {
        override def toString = "(%s.%s)".format(className, text)
        
        def javaName = text
        
        def matches(unlowerName: UnloweredMember) = unlowerName match {
            case u: ClasslessMember => (text == u.text)
            case u: Member => (this == u)
        }
        
        def inDefaultClass(className: Name.Class) = this
    }
    
    /** Names of parameters, local variables */
    sealed case class LocalVar(
        text: String
    ) extends Var {
        def javaName = text
        override def toString = text
    }
    
    /** A `Member` before lowering, at which point we may not yet
      * know the `className`. */
    sealed abstract trait UnloweredMember {
        def text: String
        
        def inDefaultClass(className: Name.Class): Member
    }
    case class ClasslessMember(text: String) extends UnloweredMember {
        def inDefaultClass(className: Name.Class) = Member(className, text)        
    }
    
    val ThisLocal = Name.LocalVar("this")
    
    val MethodLocal = Name.LocalVar("method")
    
    val VoidClass = Class(classOf[java.lang.Void])
    
    val ObjectClass = Class(classOf[java.lang.Object])

    val ArrayClass = Class(classOf[harmonic.lang.Array[_]])
    val ArrayElem = Member(ArrayClass, "E")
    
    val AbstractClass = Class(classOf[harmonic.lang.Abstract])
    val MutableClass = Class(classOf[harmonic.lang.Mutable])
    val OverrideClass = Class(classOf[java.lang.Override])

    val BlockClass = Class(classOf[harmonic.lang.Block[_, _]])
    val AsyncBlockClass = Class(classOf[harmonic.lang.AsyncBlock[_, _]])
    val BlockR = Member(BlockClass, "R")
    val BlockA = Member(BlockClass, "A")
    val BlockParent = Member(BlockClass, "Parent")
    val ValueMethod = Method(List("value"))

    val InitMethod = Method(List("<init>"))
       
}