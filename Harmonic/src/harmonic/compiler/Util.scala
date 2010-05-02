package harmonic.compiler

import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position

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
    implicit def extendedList[E](list: List[E]) = new ExtendedList(list)
    
    class ExtendedOption[E](option: Option[E]) {
        def orErr(err: => Error): CanFail[E] = option match {
            case Some(value) => Right(value)
            case None => Left(err)
        }
    }
    implicit def extendedOption[E](option: Option[E]) = new ExtendedOption(option)
    
    // ___ Debug ____________________________________________________________
    private[this] var indent: Int = 0
    
    def debug(fmt: String, args: Any*) = 
        println((" " * indent) + fmt.format(args: _*))
        
    def debugIndent[R](fmt: String, args: Any*)(func: => R) = {
        debug(fmt, args: _*)
        indent += 2
        try {
            val result = func
            if(result != ())
                debug("Result: %s", result)
            result
        } catch { case t =>
            debug("Error: %s", t)
            throw t
        } finally {
            indent -= 2
        }
    }
    
}