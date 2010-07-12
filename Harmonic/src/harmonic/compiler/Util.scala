package harmonic.compiler

import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position

import ch.ethz.intervals._
import ch.ethz.intervals.task.AbstractTask
import ch.ethz.intervals.task.ResultTask

import com.smallcultfollowing.lathos
import com.smallcultfollowing.lathos.Lathos

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
    
    class ExtendedC[C](obj: C) {
        def matchAll[R](funcs: (C => R)*): List[R] = {
            funcs.toList.flatMap { func =>
                try {
                    Some(func(obj))
                } catch {
                    case _: MatchError => None
                }
            }
        }
    }
    implicit def extendedC[C](obj: C): ExtendedC[C] = new ExtendedC(obj)
    
    class ExtendedAny(any: Any) {
        def asObj = any.asInstanceOf[java.lang.Object]
    }
    implicit def extendedAny(any: Any) = new ExtendedAny(any)    

    class ExtendedBoolean(bool: Boolean) {
        def toOption[A](a: A): Option[A] = {
            if(bool) Some(a)
            else None
        }
    }
    implicit def extendedBoolean(bool: Boolean): ExtendedBoolean = new ExtendedBoolean(bool)

    class ExtendedString(string: String) {
        def afterLast(c: Char) = {
            string.lastIndexOf(c) match {
                case -1 => string
                case i => string.substring(i+1)
            }
        }
    }
    implicit def extendedString[E](string: String): ExtendedString = new ExtendedString(string)
    
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
        
        def mapAsPrefix[F](list: List[F])(func: (E => F)): List[F] = {
            iterable.foldRight(list) { (e, l) => func(e) :: l }
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
        
        def replace(from: E, to: E) = {
            list.map { elem =>
                if(from == elem) to
                else elem
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
    
    trait DebugPage extends lathos.Page {
        @lathos.Ignore
        protected[this] var contents: List[lathos.PageContent] = Nil
        
        override def toString = getId

        override def getId = "%s[%x]".format(
            getClass.getName.afterLast('.'), 
            System.identityHashCode(this)
        )

        override def getParent = null

        override def addContent(content: lathos.PageContent) = synchronized {
            if(!contents.contains(content))
                contents = content :: contents
        }

        override def renderInLine(out: lathos.Output): Unit = {
            Lathos.renderInLine(this, out)
        }

        def renderInPage(out: lathos.Output): Unit = synchronized {
            out.startPage(this)
            
            Lathos.reflectivePageContents(this, out)
            
            contents.reverseIterator.foreach { content =>
                content.renderInPage(out)
            }

            out.endPage(this)
        }
    }

    object ScalaDataRenderer extends lathos.DataRenderer {
        
        def renderIteratorInLine(page: lathos.Page, out: lathos.Output, iter: Iterator[_]) = {
            out.outputText("(")
            
            def outputNext(withComma: Boolean) = {
                if(iter.hasNext) {
                    if(withComma) out.outputText(", ")
                    val n = iter.next
                    out.outputObject(n)
                }                
            }
            
            outputNext(false)
            outputNext(true)
            outputNext(true)
            
            if(iter.hasNext) {
                out.startLink(page)
                out.outputText(", ...")
                out.endLink(page)
            }
            out.outputText(")")            
        }
        
        case class ScalaIterable(value: Iterable[_]) extends DebugPage {
            override def renderInLine(out: lathos.Output): Unit = synchronized {
                renderIteratorInLine(this, out, value.iterator)
            }
            
            override def renderInPage(out: lathos.Output): Unit = synchronized {
                value match {
                    case value: scala.collection.Map[_, _] => {
                        out.startPage(this)
                        out.map(value)
                        out.endPage(this)
                    }
                    case _ => {
                        out.startPage(this)
                        out.list(value)
                        out.endPage(this)
                    }
                }
            }
        }
        
        case class ScalaProduct(value: Product) extends DebugPage {
            override def renderInLine(out: lathos.Output): Unit = synchronized {
                out.outputText(value.productPrefix)
                renderIteratorInLine(this, out, value.productIterator)
            }
            
            override def renderInPage(out: lathos.Output): Unit = synchronized {
                out.startPage(this)
                out.table {
                    out.row("Prefix", value.productPrefix)
                    value.productIterator.zipWithIndex.foreach { case (v, i) =>
                        out.row(i.asObj, v.asObj)
                    }
                }
                out.endPage(this)
            }
        }
        
        override def addToLine(line: lathos.Line, value: Object): Boolean = {
            value match {
                case value: Iterable[_] => {
                    line.addContent(ScalaIterable(value))
                    true
                }
                
                case value: Type => {
                    false
                }
                
                case value: MethodSignature[_] => {
                    false
                }
                
                case value: Pattern.Anon => {
                    false
                }
                
                case value: Path.Ref => {
                    false
                }
                
                case value: Name.Any => {
                    false
                }
                
                case value: Ast.Node => {
                    false
                }
                
                case value: Product => {
                    line.addContent(ScalaProduct(value))
                    true
                }
                
                case _ => false
            }
        }
        
    }
    
    class ExtendedServer(server: lathos.LathosServer) {
        def addDefaultRenderers = {
            server.addDataRenderer(ScalaDataRenderer)
            server.addDataRenderer(new lathos.ThrowableDataRenderer())
        }
        
        def topLevelPage(id: String, title: Object*) = {
            val context = server.context
            val page = context.pushTopLevel(id, title: _*)
            context.pop(page)
            context.log("See ", page)
            server.registerPage(page)
            page
        }
        
        def pageForClass(name: Name.Class): lathos.Page = topLevelPage(name.toString, "Class ", name)
        
        def contextForPage(page: lathos.Page) = {
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
    }
    implicit def extendedServer(server: lathos.LathosServer): ExtendedServer = new ExtendedServer(server)

    class ExtendedContext(context: lathos.Context) {
        def indent[R](args: Object*)(func: => R) = {
            
            val indentPage = Lathos.newPage(context.server, context.topPage, null, args: _*)
            val indentLine = context.log(context.link(indentPage, "\u25B2"))
            context.push(indentPage)
            
            try {
                context.append(indentLine, args: _*)
                
                val result = func

                context.log("Result: ", result.asObj)
                context.pop(indentPage)
                context.append(indentLine, " (Result: ", result.asObj, ")")
                
                result
            } catch { 
                case t => {
                    context.log("Error: ", t)
                    context.pop(indentPage)
                    context.append(indentLine, " (Error: ", t, ")")
                    
                    throw t                    
                }
            }
            
        }
    }
    implicit def extendedContext(context: lathos.Context): ExtendedContext = new ExtendedContext(context)
    
    class ExtendedOutput(out: lathos.Output) {
        
        def par(func: => Unit) {
            out.startPar
            func
            out.endPar
        }
        
        def bolded(func: => Unit) {
            out.startBold
            func
            out.endBold
        }
        
        def row(func: => Unit) {
            out.startRow
            func
            out.endRow
        }
        
        def column(func: => Unit) {
            out.startColumn
            func
            out.endColumn
        }
        
        def table(func: => Unit) {
            out.startTable
            func
            out.endTable
        }
        
        def row(objs: Object*) {
            Lathos.row(out, objs: _*)
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
            table {
                data.foreach { item =>
                    row {
                        column {
                            out.outputObject(item.asObj)
                        }
                    }
                }                
            }
        }
        
        def map(data: Iterable[(Any, Any)]) {
            table {
                data.foreach { case (col1, col2) =>
                    out.row(
                        col1.asInstanceOf[Object], 
                        col2.asInstanceOf[Object]
                    )
                }                
            }
        }
        
    }
    implicit def extendedOutput(output: lathos.Output): ExtendedOutput = new ExtendedOutput(output)
    
    def usingLog[R](log: lathos.Context)(func: => R) = {
        val oldLog = Lathos.setContext(log)
        try {
            func
        } finally {
            Lathos.setContext(oldLog)
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
            Lathos.context.log(
                label, "completed in ", 
                elapsedMilli.asObj, " ms, or ",
                elapsedMin.asObj, " min ", 
                (elapsedSec % 60).asObj, " sec ", 
                (elapsedMilli % 1000).asObj, " ms"
            )
        }
    }
    
    // ___ Intervals ________________________________________________________
    
    case class ExtendedInterval(inter: Interval, implicit val global: Global) {
        def subinterval[R](
            name: String,
            parentPage: lathos.Page,
            during: List[Interval] = Nil,
            before: List[Point] = Nil,
            after: List[Point] = Nil,
            schedule: Boolean = true
        )(
            func: (Interval => Unit)
        ): AsyncInterval = {
            val creatorPage = Lathos.context.topPage
            
            val interPage = Lathos.newPage(
                global.debugServer, 
                parentPage,
                name.afterLast('.'),
                "Interval ", name
            )
            
            val creatorLog = global.debugServer.contextForPage(parentPage)
            creatorLog.log("Phase ", interPage)

            val info = global.intervalsPage.registerInterval(parentPage, interPage)
            
            val result = inter.newAsyncChild(new AbstractTask(name) {
                override def run(current: Interval): Unit = {
                    val log = global.debugServer.context
                    usingLog(log) {
                        try {
                            info.setRunning
                            
                            log.push(interPage)
                            log.log("Interval object ", current)
                            log.log("Created by ", creatorPage)
                            func(current)
                            log.pop(interPage)
                            
                            info.setCompleted
                        } catch { case t => 
                            log.push(global.errorsPage)
                            log.log("Interval ", interPage, " threw ", t)
                            log.pop(global.errorsPage)
                            
                            info.setThrew(t)
                            throw t
                        }
                    }
                }
            })

            info.setInterval(result)
            
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
    implicit def extendedInterval(inter: Interval)(implicit global: Global) = ExtendedInterval(inter, global)
    
    def inlineInterval[R](name: String)(func: (Interval => R)): R = {
        val log = Lathos.context
        assert(log != null)
        Intervals.inline(new ResultTask[R](name) {
            def compute(subinterval: Interval) = {
                usingLog(log) {
                    log.indent("Subinterval ", name) {
                        func(subinterval)                    
                    }
                }
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