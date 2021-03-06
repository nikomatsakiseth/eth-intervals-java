package harmonic.compiler

import Util._
import java.lang.reflect.{Modifier => jModifier}

object Modifier {
    
    abstract class Mod(val bit: Int, val jBit: Int, val name: Name.Class)
    case object Abstract extends Mod(1, jModifier.ABSTRACT, Name.AbstractClass)
    case object Mutable extends Mod(2, 0, Name.MutableClass)
    case object Override extends Mod(4, 0, Name.OverrideClass)
    case object Static extends Mod(8, jModifier.STATIC, Name.VoidClass)
    case object Unscheduled extends Mod(16, 0, Name.UnscheduledClass)
    
    val All = List(Abstract, Mutable, Override, Static, Unscheduled)
    
    case class Set(mods: Int) {
        override def toString = "ModSet(%s)".format(toList.mkString(", "))
        def contains(mod: Mod) = mods.containsBits(mod.bit)
        def +(mod: Mod) = Set(mods | mod.bit)
        def toList = All.filter(contains)
        
        def isAbstract = contains(Abstract)
        def isNotAbstract = !contains(Abstract)
        
        def isOverride = contains(Override)
        def isNotOverride = !contains(Override)
        
        def isStatic = contains(Static)
        def isNotStatic = !contains(Static)
        
        def isMutable = contains(Mutable)
        def isNotMutable = !contains(Mutable)
        
        def isUnscheduled = contains(Unscheduled)
        def isNotUnscheduled = !contains(Unscheduled)
    }
    
    object Set {
        val empty = Set(0)
        def apply(mods: List[Mod]): Set = Set(mods.foldLeft(0)(_ | _.bit))
    }
    
    def forJavaModifiers(bits: Int) = {
        Set(All.filter(mod => bits.containsBits(mod.jBit)))
    }
    
    def forClass(cls: java.lang.Class[_]) = {
        forJavaModifiers(cls.getModifiers)
    }

    def forMember(mem: java.lang.reflect.Member) = {
        forJavaModifiers(mem.getModifiers)
    }

    def forAnnotationsNamed(annotationNames: List[Name.Class]) = {
        Set(All.filter(ann => annotationNames.contains(ann.name)))
    }
    
    def forResolvedAnnotations(annotations: List[Ast.Resolve.Annotation]) = {
        forAnnotationsNamed(annotations.map(_.name.name))
    }

    def forLoweredAnnotations(annotations: List[Ast.Lower.Annotation]) = {
        forAnnotationsNamed(annotations.map(_.name.name))
    }

}