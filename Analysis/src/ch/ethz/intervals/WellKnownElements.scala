package ch.ethz.intervals

import ch.ethz.intervals.guard.Guard

import javax.lang.model.util.Elements
import javax.lang.model.util.{ElementFilter => EF}
import javax.lang.model.util.Types

import scala.collection.jcl.Conversions._

class WellKnownElements(elements: Elements, types: Types) {
    class TypeInfo(cls: Class[_]) {
        val elem = elements.getTypeElement(cls.getName)
        val name = elem.getQualifiedName
        val ty = elem.asType
        
        def method(mname: String, argTypes: TypeInfo*) = {
            val argTypesList = argTypes.toList
            EF.methodsIn(elem.getEnclosedElements).find(mem =>
                mem.getSimpleName.contentEquals(mname) && 
                mem.getParameters.size == argTypes.length &&
                argTypesList.zip(mem.getParameters.toList).forall { case (exp, act) =>
                    types.isSameType(exp.ty, act.asType)
                }
            ).get
        }
    }

    val Intervals = new TypeInfo(classOf[Intervals])
    val Interval = new TypeInfo(classOf[Interval])
    val Point = new TypeInfo(classOf[Point])
    val Guard = new TypeInfo(classOf[Guard])
    val Object = new TypeInfo(classOf[Object])
    
    val addHbIntervalInterval = Intervals.method("addHb", Interval, Interval)
    val addHbIntervalPoint = Intervals.method("addHb", Interval, Point)
    val addHbPointInterval = Intervals.method("addHb", Point, Interval)
    val addHbPointPoint = Intervals.method("addHb", Point, Point)
    val addHb = Set(addHbIntervalInterval, addHbIntervalPoint, addHbPointInterval, addHbPointPoint)
}