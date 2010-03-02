package ch.ethz.intervals

import scala.collection.JavaConversions._

import javax.lang.model.element.ExecutableElement
import javax.lang.model.element.Element
import javax.lang.model.util.Elements
import javax.lang.model.util.{ElementFilter => EF}
import javax.lang.model.util.Types

class WellKnownElements(elements: Elements, types: Types) {
    class TypeInfo[C](val cls: Class[C]) {
        val elem = elements.getTypeElement(cls.getName)
        val name = elem.getQualifiedName
        val ty = elem.asType
        
        def field(fname: String) = {
            EF.fieldsIn(elem.getEnclosedElements).find(mem =>
                mem.getSimpleName.contentEquals(fname)
            ).get
        }
        
        private[this] def matches(mname: String, argTypes: List[TypeInfo[_]])(mem: ExecutableElement) = {
            mem.getSimpleName.contentEquals(mname) && 
            mem.getParameters.size == argTypes.length && {
                argTypes.zip(mem.getParameters.toList).forall { case (exp, act) =>
                    types.isSameType(
                        types.erasure(exp.ty), 
                        types.erasure(act.asType))
                }
            }            
        }
        
        def method(mname: String, argTypes: TypeInfo[_]*): ExecutableElement = {
            EF.methodsIn(elem.getEnclosedElements).find(matches(mname, argTypes.toList)).get
        }
        
        def ctor(argTypes: TypeInfo[_]*): ExecutableElement = {
            EF.constructorsIn(elem.getEnclosedElements).find(matches("<init>", argTypes.toList)).get
        }
        
        def isSuperElementOf(otherElem: Element) = {
            val otherTy = otherElem.asType
            types.isSubtype(types.erasure(otherTy), types.erasure(ty))
        }
    }

    val Intervals = new TypeInfo(classOf[Intervals])
    val Interval = new TypeInfo(classOf[Interval])
    val Point = new TypeInfo(classOf[Point])
    val Guard = new TypeInfo(classOf[ch.ethz.intervals.guard.Guard])
    val Object = new TypeInfo(classOf[Object])
    val DefinesGhost = new TypeInfo(classOf[ch.ethz.intervals.quals.DefinesGhost])
    val Requires = new TypeInfo(classOf[ch.ethz.intervals.quals.Requires])
    val Subinterval = new TypeInfo(classOf[ch.ethz.intervals.Subinterval])
    
    val InlineTask = new TypeInfo(classOf[ch.ethz.intervals.InlineTask[_]])
    val VoidInlineTask = new TypeInfo(classOf[ch.ethz.intervals.VoidInlineTask])
    val inlineTaskClasses = Set(InlineTask, VoidInlineTask)
    
    val addHbIntervalInterval = Intervals.method("addHb", Interval, Interval)
    val addHbIntervalPoint = Intervals.method("addHb", Interval, Point)
    val addHbPointInterval = Intervals.method("addHb", Point, Interval)
    val addHbPointPoint = Intervals.method("addHb", Point, Point)
    val addHb = Set(addHbIntervalInterval, addHbIntervalPoint, addHbPointInterval, addHbPointPoint)
    
    val toStringEelem = Object.method("toString")
    
    val inlineTaskCtor = InlineTask.ctor()
    val inlineTaskInit = InlineTask.method("init", Interval)
    val inlineTaskRun = InlineTask.method("run", Interval)
    
    val voidInlineTaskCtor = VoidInlineTask.ctor()
    val voidInlineTaskInit = VoidInlineTask.method("init", Interval)
    val voidInlineTaskRun = VoidInlineTask.method("run", Interval)

    
    val inlineResult = Intervals.method("inline", InlineTask)
    val inlineVoid = Intervals.method("inline", VoidInlineTask)
    val inline = Set(inlineResult, inlineVoid)
    
    val ofClass = DefinesGhost.method("ofClass")
    val useByDefault = DefinesGhost.method("useByDefault")
}