package harmonic.compiler

import Util._

object Name {
    
    sealed abstract class Any {
        def unapply(any: Any) = (this == any)
    }
    
    sealed abstract class Qual extends Any {
        def toInternalPrefix: String
        
        /** Tag used in generated fields etc: `String` == `$Harmonic$java$lang$String$` */
        def toTag: String
        
        def toPrefix: String
        
        /** User-visible, absolute name. `Map.Entry` == `java.util.Map.Entry` */
        def toString: String
        
        def asClassName: Option[Class]
        
        def toAnnPrefix: String
        
        /** Name parsable by `AnnParser`. `Map.Entry` == `java.util.Map$Entry` */
        def toAnnString: String
    }
    
    sealed abstract class Package extends Qual {
        def asClassName = None
    }
    
    case object Root extends Package {
        override def toInternalPrefix = ""
        override def toTag = "$Harmonic$"
        override def toPrefix = ""
        override def toString = "<root>"
        override def toAnnPrefix = ""
        override def toAnnString = "<root>"
    }
    
    final case class Subpackage(
        base: Package,
        name: String
    ) extends Package {
        override def toInternalPrefix = base.toInternalPrefix + name + "/"
        override def toTag = base.toTag + name + "$"
        override def toPrefix = toString + "."
        override def toString = base.toPrefix + name
        override def toAnnPrefix = toString + "."
        override def toAnnString = base.toAnnPrefix + name
    }
    
    final case class Class(
        base: Qual,
        name: String
    ) extends Qual {
        def is(name: Name.Class) = (this == name)
        
        override def toInternalPrefix = internalName + "$"
        
        override def toTag = base.toTag + name + "$"
        
        override def toPrefix = toString + "."
        
        override def toAnnPrefix = toString + "$"
        
        override def asClassName = Some(this)
        
        def toType = Type.Class(this, Nil)
        
        /** Relative path leading to the `.class` file for this class, but without 
          * any extension. `String` == `java/lang/String` */
        def relPath: String = internalName
        
        /** So-called "Internal name" used in JVM: `String` == `java/lang/String` */
        def internalName = base.toInternalPrefix + name
        
        override def toString = base.toPrefix + name
        
        override def toAnnString = base.toAnnPrefix + name
        
        /** Creates a new class name in same package but with the given suffix. */
        def withSuffix(suffix: String) = Class(base, name + suffix)
    }
    
    object Package {
        
        def apply(str: String): Package = {
            val comp = str.split('.')
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
    final case class Method(
        parts: List[String]
    ) extends Any {
        def javaName = parts.mkString("$")
        
        def is(name: Name.Method) = (this == name)
        
        override def toString = parts.mkString("", "()", "()")
    }
    
    /** Names of variables */
    sealed abstract class Var extends Any {
        // The shorthand form the user would use to refer
        // to this variable.  For example, a field "foo"
        // in a class "Bar" would return "foo".
        def text: String
    }
    
    /** Fully specified names of members (except for methods) */
    final case class Member(
        className: Name.Class,
        text: String
    ) extends Var with UnloweredMember {
        override def toString = "(%s.%s)".format(className, text)
        
        def is(otherName: Name.Member) = (this == otherName)
        
        def toAnnString = "(%s#%s)".format(className, text)
        
        def matches(unlowerName: UnloweredMember) = unlowerName match {
            case u: ClasslessMember => (text == u.text)
            case u: Member => (this == u)
        }
        
        def inDefaultClass(className: Name.Class) = this
    }
    
    /** Names of parameters, local variables */
    final case class LocalVar(
        text: String
    ) extends Var {
        def toPath = Path.Local(this)
        def is(otherName: LocalVar) = (this == otherName)
        override def toString = text
    }
    
    /** A `Member` before lowering, at which point we may not yet
      * know the `className`. */
    sealed abstract trait UnloweredMember extends Any {
        def text: String
        
        def inDefaultClass(className: Name.Class): Member
    }
    final case class ClasslessMember(text: String) extends UnloweredMember {
        def inDefaultClass(className: Name.Class) = Member(className, text)        
    }
    
    // The special variable `this` refers to the current receiver.
    val ThisLocal = Name.LocalVar("this")
    
    // The special variable `method` refers to the interval for
    // the current method activation.
    val MethodLocal = Name.LocalVar("method")

    // The special variable `final` refers to a guard
    // that never permits writes.
    val FinalLocal = Name.LocalVar("final")

    // Object: base class of all objects!
    //
    // Implicitly defines the Wr ghost, conventionally used to control
    // when an object is mutable.
    val ObjectClass = Class(classOf[java.lang.Object])
    val Wr = Member(ObjectClass, "Wr")     // ghost field "Wr" defined in all objects
    val Init = Member(ObjectClass, "Init") // ghost field "Init" defined in all objects

    // Void: an interface that is never implemented. 
    // Treated specially in that blocks which have Void 
    // type automatically return null.
    val VoidClass = Class(classOf[java.lang.Void])
    
    // Guard: interface for field and local variable guards
    val GuardClass = Class(classOf[ch.ethz.intervals.guard.Guard])
    
    // Interval, AsyncInterval, InlineInterval: from interval runtime
    val RoIntervalClass = Class(classOf[ch.ethz.intervals.RoInterval])
    val IntervalClass = Class(classOf[ch.ethz.intervals.Interval])
    val AsyncIntervalClass = Class(classOf[ch.ethz.intervals.AsyncInterval])
    val InlineIntervalClass = Class(classOf[ch.ethz.intervals.InlineInterval])

    // Point: from interval runtime
    val RoPointClass = Class(classOf[ch.ethz.intervals.RoPoint])
    val PointClass = Class(classOf[ch.ethz.intervals.Point])

    // Lock: from interval runtime
    val RoLockClass = Class(classOf[ch.ethz.intervals.RoLock])
    val LockClass = Class(classOf[ch.ethz.intervals.Lock])

    // HarmonicTask: convenience class used for our harmonic tasks
    val HarmonicTaskClass = Class(classOf[harmonic.runtime.HarmonicTask])

    // Array: Class used to represent arrays of objects
    val ArrayClass = Class(classOf[harmonic.lang.Array[_]])
    val ArrayElem = Member(ArrayClass, "E")
    
    // Abstract, Mutable, Override, Unscheduled: Java annotations significant
    // to the compiler.
    val AbstractClass = Class(classOf[harmonic.lang.Abstract])
    val MutableClass = Class(classOf[harmonic.lang.Mutable])
    val OverrideClass = Class(classOf[java.lang.Override])
    val UnscheduledClass = Class(classOf[harmonic.lang.Unscheduled])

    // Block, AsyncBlock: classes used to implement blocks in Harmonic
    val BlockClass = Class(classOf[harmonic.lang.Block[_, _]])
    val AsyncBlockClass = Class(classOf[harmonic.lang.AsyncBlock[_, _]])
    val BlockR = Member(BlockClass, "R")
    val BlockA = Member(BlockClass, "A")
    val BlockParent = Member(BlockClass, "Parent")
    val ValueMethod = Method(List("value"))
    
    // Return: the exception thrown internally to return out of a block
    val ReturnClass = Class(classOf[harmonic.runtime.Return])

    // <init>: The name of the constructor in Java.
    val InitMethod = Method(List("<init>"))
       
}