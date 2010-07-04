package harmonic.compiler

import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position

import ch.ethz.intervals._
import ch.ethz.intervals.task.AbstractTask
import ch.ethz.intervals.task.ResultTask

import com.smallcultfollowing.lathos.model.Context
import com.smallcultfollowing.lathos.model.Page
import com.smallcultfollowing.lathos.model.LathosServer
import com.smallcultfollowing.lathos.model.Output

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
    
    // ___ Extensions to Collection Classes and Other Miscellany ____________
    
    class ExtendedAny(any: Any) {
        def asObj = any.asInstanceOf[java.lang.Object]
    }
    implicit def extendedAny(any: Any) = new ExtendedAny(any)    

    class ExtendedString[E](string: String) {
        def afterLast(c: Char) = {
            string.lastIndexOf(c) match {
                case -1 => string
                case i => string.substring(i+1)
            }
        }
    }
    implicit def extendedString[E](string: String) = new ExtendedString(string)
    
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
    // Extend the Lathos debugging library.

    class ExtendedServer(server: LathosServer) {
        def topLevelPage(id: String, title: Object*) = {
            val context = server.context
            val page = context.pushTopLevel(id, title: _*)
            context.pop(page)
            context.log("See ", page)
            server.registerPage(page)
            page
        }
        
        def pageForClass(name: Name.Class): Page = topLevelPage(name.toString, "Class ", name)
        
        def contextForPage(page: Page) = {
            val context = server.context
            context.push(page)
            context
        }
        
        def contextForPageTitled(id: String, title: Object*) = {
            val context = server.context
            val page = context.pushTopLevel(id, title: _*)
            context.pop(page)
            context.log("See ", page)
            context.push(page)
            context
        }
        
        def contextForInter(classPage: Page, inter: Interval) = {
            val context = server.context
            val tag = inter.toString.afterLast('.')
            context.push(classPage)
            context.pushChild(tag, tag)
            context
        }
    }
    implicit def extendedServer(server: LathosServer): ExtendedServer = new ExtendedServer(server)

    class ExtendedContext(context: Context) {
        def indent[R](args: Object*)(func: => R) = {
            val page = context.pushChild(null, args: _*)
            try {
                val result = func
                if(result != ())
                    context.log("Result: ", result.asInstanceOf[Object])
                result
            } catch { case t =>
                context.log("Error: ", t)
                throw t
            } finally {
                context.pop(page)
            }
        }
    }
    implicit def extendedContext(context: Context): ExtendedContext = new ExtendedContext(context)
    
    class ExtendedOutput(out: Output) {
        
        def row(objs: Object*) {
            com.smallcultfollowing.lathos.model.Util.row(out, objs: _*)
        }
        
        def subpage(title: String)(func: => Unit) {
            out.startPage(null)
            out.startBold
            out.outputText(title)
            out.endBold
            func
            out.endPage(null)
        }
        
        def list(data: Iterable[Any]) {
            out.startTable
            
            data.foreach { row =>
                out.startRow
                out.startColumn
                out.outputText(row.toString)
                out.endColumn
                out.endRow
            }
            
            out.endTable            
        }
        
        def map(data: Iterable[(Any, Any)]) {
            out.startTable
            
            data.foreach { case (col1, col2) =>
                out.row(
                    col1.asInstanceOf[Object], 
                    col2.asInstanceOf[Object]
                )
            }
            
            out.endTable
        }
        
    }
    implicit def extendedOutput(output: Output): ExtendedOutput = new ExtendedOutput(output)
    
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
                    throw r.allErrors.iterator.next;
                }
                case r: RethrownException => {
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