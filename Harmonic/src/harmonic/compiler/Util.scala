package harmonic.compiler

import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position

import scala.collection.Set

object Util {
    def javaReaderFromPath(path: String) = javaReaderFromFile(new java.io.File(path))
    def javaReaderFromFile(file: java.io.File) = new java.io.FileReader(file)
    
    def withPosOf[P <: Positional, Q <: Positional](from: P, to: Q): Q = {
        to.setPos(from.pos); to
    }
    
    def sameLength(lst1: List[_], lst2: List[_]) = (lst1.length == lst2.length)
    
    // ___ Extensions to Collection Classes _________________________________
    
    class ExtendedIterable[E](iterable: Iterable[E]) {
        def firstSome[F](func: (E => Option[F])) = iterable.foldLeft[Option[F]](None) {
            case (None, elem) => func(elem)
            case (result, _) => result
        }
        
        def cross[J](js: Iterable[J]) = 
            for(i <- iterable.view; j <- js.view) yield (i,j)
            
        def intersects(seq: Seq[E]): Boolean =
            iterable.exists(seq.contains)
    }
    implicit def extendedIterable[E](iterable: Iterable[E]) = new ExtendedIterable(iterable)
    
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