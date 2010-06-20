package harmonic.compiler

import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position

import ch.ethz.intervals._
import ch.ethz.intervals.task.AbstractTask
import ch.ethz.intervals.task.ResultTask

import scala.collection.Set

import Error.CanFail

object Util {
    def javaReaderFromPath(path: String) = javaReaderFromFile(new java.io.File(path))
    def javaReaderFromFile(file: java.io.File) = new java.io.FileReader(file)
    
    def withPosOf[P <: Positional, Q <: Product with Positional](from: P, to: Q): Q = {
        to.setPos(from.pos)
        if(to.pos == from.pos) { // we may have changed it, update sub-items
            to.productIterator.foreach { 
                case to0: (Product with Positional) => withPosOf(from, to0)
                case _ => ()
            }
        }
        to
    }
    
    def withPosOfOpt[P <: Positional, Q <: Product with Positional](from: P, to: Option[Q]): Option[Q] = {
        to.map(withPosOf(from, _))
    }
    
    def withPosOfR[P <: Positional, Q, R <: Product with Positional](from: P, to: Either[Q, R]): Either[Q, R] = {
        to match {
            case Left(q) => Left(q)
            case Right(r) => Right(withPosOf(from, r))
        }
    }
    
    def sameLength(lst1: List[_], lst2: List[_]) = (lst1.length == lst2.length)
    
    // ___ Extensions to Collection Classes _________________________________
    
    class ExtendedIterable[E](iterable: Iterable[E]) {
        def firstSome[F](func: (E => Option[F])) = {
            iterable.foldLeft[Option[F]](None) {
                case (None, elem) => func(elem)
                case (result, _) => result
            }
        }

        def firstRight[L,R](left0: L)(func: ((L, E) => Either[L,R])) = {
            iterable.foldLeft[Either[L,R]](Left(left0)) {
                case (Left(left), elem) => func(left, elem)
                case (result, _) => result
            }
        }
        
        def cross[J](js: Iterable[J]) = 
            for(i <- iterable.view; j <- js.view) yield (i,j)
            
        def intersects(seq: Seq[E]): Boolean =
            iterable.exists(seq.contains)
    }
    implicit def extendedIterable[E](iterable: Iterable[E]) = new ExtendedIterable(iterable)
    
    class ExtendedList[E](list: List[E]) {
        def mapContext[C,F](context: C, func: (E => (C, F))): (C, List[F]) = {
            list match {
                case List() => (context, List())
                case hd :: tl => {
                    val (context1, hd1) = func(hd)
                    val (context2, tl1) = tl.mapContext(context, func)
                    (context2, hd1 :: tl1)
                }
            }
        }
    }
    implicit def extendedList[E](list: List[E]): ExtendedList[E] = new ExtendedList(list)
    
    class ExtendedOption[E](option: Option[E]) {
        def orErr(err: => Error): CanFail[E] = option match {
            case Some(value) => Right(value)
            case None => Left(err)
        }
        
        def ifNone(func: => Option[E]) = option match {
            case Some(e) => option
            case None => func
        }
    }
    implicit def extendedOption[E](option: Option[E]): ExtendedOption[E] = new ExtendedOption(option)
    
    // ___ Debug ____________________________________________________________
    //
    // The output of all threads gets mixed together. You can grep for a 
    // particular thread id to sort it out. In TextMate, use 
    // Command-Opt-R with "grep ^001:" or what have you.
    
    class DebugData(val threadId: Int) {
        var indent: Int = 0
        
        def addIndent { indent += 2}
        def removeIndent { indent -= 2}
    }
    
    private[this] val local = new java.lang.ThreadLocal[DebugData]() {
        private[this] var maxThreadId = 0
        override def initialValue() = synchronized {
            val threadId = maxThreadId
            maxThreadId += 1
            new DebugData(threadId)
        }
    }
    
    def debug(fmt: String, args: Any*) = {
        val data = local.get
        val str = "%03x: ".format(data.threadId) + (" " * data.indent) + fmt.format(args: _*)
        Util.synchronized { println(str) }
    }
    
    def debugAddIndent {
        val data = local.get
        data.addIndent
    }
        
    def debugRemoveIndent {
        val data = local.get
        data.removeIndent
    }
        
    def debugIndent[R](fmt: String, args: Any*)(func: => R) = {
        val data = local.get
        debug(fmt, args: _*)
        data.addIndent
        try {
            val result = func
            if(result != ())
                debug("Result: %s", result)
            result
        } catch { case t =>
            debug("Error: %s", t)
            throw t
        } finally {
            data.removeIndent
        }
    }
    
    // ___ Profiling ________________________________________________________
    
    def measure[R](label: String)(func: => R) = {
        val start = System.currentTimeMillis
        try {
            func
        } finally {
            val elapsedMilli = System.currentTimeMillis - start
            val elapsedSec = elapsedMilli / 1000
            val elapsedMin = elapsedSec / 60
            Util.synchronized {
                println("%s: %s ms == %s min %s sec %s ms".format(
                    label,
                    elapsedMilli,
                    elapsedMin,
                    elapsedSec % 60,
                    elapsedMilli % 1000
                ))
            }
        }
    }
    
    // ___ Intervals ________________________________________________________
    
    case class ExtendedInterval(inter: Interval) {
        def subinterval[R](
            during: List[Interval] = Nil,
            before: List[Point] = Nil,
            after: List[Point] = Nil,
            name: String = null,
            schedule: Boolean = true
        )(
            func: (Interval => Unit)
        ): AsyncInterval = {
            val result = inter.newAsyncChild(new AbstractTask(name) {
                override def run(current: Interval): Unit = func(current)
            })
            after.foreach(pnt => Intervals.addHb(pnt, result.getStart))
            before.foreach(pnt => Intervals.addHb(result.getEnd, pnt))
            during.foreach(inter => Intervals.addHb(inter.getStart, result.getStart))
            during.foreach(inter => Intervals.addHb(result.getEnd, inter.getEnd))
            if(schedule) result.schedule()
            
            result
        }
        
        def join() = {
            try {
                Intervals.inline(new AbstractTask() {
                    override def toString = 
                        "join(%s)".format(inter)
                    override def attachedTo(inlineInterval: Interval) = 
                        Intervals.addHb(inter.getEnd, inlineInterval.getStart)
                    override def run(inlineInterval: Interval) = 
                        ()
                })                       
            } catch {
                // Rethrow wrapped exceptions (if there is exactly one):
                case r: RethrownException if r.allErrors.size == 1 => {
                    debug("rethrown exc w/ %d errors", r.allErrors.size)
                    throw r.allErrors.iterator.next;
                }
                case r: RethrownException => {
                    debug("rethrown exc w/ %d errors", r.allErrors.size)
                    throw r;
                }
            }
        }
    }
    implicit def extendedInterval(inter: Interval) = ExtendedInterval(inter)
    
    def inlineInterval[R](name: String)(func: (Interval => R)): R = {
        Intervals.inline(new ResultTask[R](name) {
            def compute(subinterval: Interval) = {
                func(subinterval)
            }
        })
    }
    
    // ___ AnyOf3 ___________________________________________________________
    //
    // Like Either, but with three choices.
    
    abstract trait AnyOf3[+A, +B, +C]
    case class OneOf3[+A, +B, +C](value: A) extends AnyOf3[A, B, C]
    case class TwoOf3[+A, +B, +C](value: B) extends AnyOf3[A, B, C]
    case class ThreeOf3[+A, +B, +C](value: C) extends AnyOf3[A, B, C]
    
}