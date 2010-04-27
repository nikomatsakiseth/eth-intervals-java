package harmonic.compiler

import java.lang.reflect.{Modifier => jModifier}

object Modifier {
    
    abstract class Mod(val bit: Int, val jBit: Int, val name: Name.Qual)
    case object Abstract extends Mod(1, jModifier.ABSTRACT, Name.AbstractQual)
    case object Mutable extends Mod(2, 0, Name.MutableQual)
    case object Override extends Mod(4, 0, Name.OverrideQual)
    
    val All = List(Abstract, Mutable, Override)
    
    case class Set(mods: Int) {
        override def toString = "ModSet(%s)".format(toList.mkString(", "))
        def contains(mod: Mod) = (mods & mod.bit) != 0
        def +(mod: Mod) = Set(mods | mod.bit)
        def toList = All.filter(contains)
        
        def isAbstract = contains(Abstract)
        def isNotAbstract = !contains(Abstract)
        
        def isOverride = contains(Override)
        def isNotOverride = !contains(Override)
    }
    
    object Set {
        val empty = Set(0)
        def apply(mods: List[Mod]): Set = Set(mods.foldLeft(0)(_ | _.bit))
    }
    
    def forJavaModifiers(bits: Int) = {
        Set(All.filter(mod => (bits & mod.jBit) != 0))
    }
    
    def forClass(cls: java.lang.Class[_]) = {
        forJavaModifiers(cls.getModifiers)
    }

    def forMember(mem: java.lang.reflect.Member) = {
        forJavaModifiers(mem.getModifiers)
    }

    def forAnnotationsNamed(annotationNames: List[Name.Qual]) = {
        Set(All.filter(ann => annotationNames.contains(ann.name)))
    }
    
    def forResolvedAnnotations(annotations: List[Ast.Resolve.Annotation]) = {
        forAnnotationsNamed(annotations.map(_.name.qualName))
    }

    def forLoweredAnnotations(annotations: List[Ast.Lower.Annotation]) = {
        forAnnotationsNamed(annotations.map(_.name.qualName))
    }

}