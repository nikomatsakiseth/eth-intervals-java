package harmonic.compiler

object Name {
    
    sealed abstract Qual {
        def asRelPath: String = internalName
        def toInternalPrefix: String
        def toPrefix: String
    }
    
    sealed abstract Package extends Qual
    
    case object Root extends Package {
        def toInternalPrefix = ""
        def toPrefix = ""
        override def toString = "<root>"
    }
    
    case class Subpackage(
        base: PackageQual,
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
        def internalName = base.toInternalPrefix + name
        def toInternalPrefix = internalName + "$"
        def toPrefix = base.toPrefix + name + "."
        override def toString = base.toPrefix + name
        def withSuffix(suffix: String) = ClassQual(base, name + suffix)
    }
    
    object Package {
        
        def apply(str: String): Package = {
            val comp = pkg.getName.split(".")
            comp.foldLeft(Root) {
                case (q, n) => SubpackageQual(q, n)
            }
        }
        
        def apply(pkg: java.lang.Package): Package = {
            if(pkg == null) Root
            else apply(pkg.getName)
        }
        
    }
    
    object Class {
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
                val outerQual = apply(cls.getDeclaringClass)
                ClassQual(outerQual, cls.getSimpleName)
            }
            
            // Handle top-level classes:
            case _ => {
                val packageQual = PackageQual(cls.getPackage)
                ClassQual(packageQual, cls.getSimpleName)
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
    
    /** Names of fields, type variables, ghosts */
    sealed case class MemberVar(
        className: Name.Qual,
        text: String
    ) extends Var {
        override def toString = "(%s.%s)".format(className, text)
    }
    
    /** Names of parameters, local variables */
    sealed case class LocalVar(
        text: String
    ) extends Var {
        override def toString = text
    }
    
    private[this] val lang = "harmonic.lang."
    
    val ThisVar = Name.LocalVar("this")
    
    val MethodVar = Name.LocalVar("method")
    
    val VoidQual = Class(classOf[java.lang.Void])
    
    val ObjectQual = Class(classOf[java.lang.Object])

    val ArrayQual = Qual(lang + "Array")
    val ArrayElem = MemberVar(ArrayQual, "E")
    
    val AbstractQual = Qual(lang + "Abstract")
    val MutableQual = Qual(lang + "Mutable")
    val OverrideQual = Class(classOf[java.lang.Override])

    val BlockQual = Class(classOf[harmonic.lang.Block[_, _]])
    val AsyncBlockQual = Class(classOf[harmonic.lang.AsyncBlock[_, _]])
    val RVar = MemberVar(BlockQual, "R")
    val AVar = MemberVar(BlockQual, "A")
    val IntervalTmplParent = MemberVar(BlockQual, "Parent")
    val ValueMethod = Method(List("value"))

    val InitMethod = Method(List("<init>"))
       
}