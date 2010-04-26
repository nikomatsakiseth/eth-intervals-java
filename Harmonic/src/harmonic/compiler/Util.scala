package harmonic.compiler

import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position

object Util {
    def javaReaderFromPath(path: String) = javaReaderFromFile(new java.io.File(path))
    def javaReaderFromFile(file: java.io.File) = new java.io.FileReader(file)
    
    def withPosOf[P <: Positional, Q <: Positional](from: P, to: Q): Q = {
        to.setPos(from.pos); to
    }
    
    def sameLength(lst1: List[_], lst2: List[_]) = (lst1.length == lst2.length)
    
    def debug(fmt: String, args: Any*) = println(fmt.format(args: _*))
    
    // ___ Extensions to Collection Classes _________________________________
    
    class ExtendedIterable[E](iterable: Iterable[E]) {
        def firstSome[F](func: (E => Option[F])) = iterable.foldLeft[Option[F]](None) {
            case (None, elem) => func(elem)
            case (result, _) => result
        }
        
        def cross[J](js: Iterable[J]) = 
            for(i <- iterable.view; j <- js.view) yield (i,j)
    }
    implicit def extendedIterable[E](iterable: Iterable[E]) = new ExtendedIterable(iterable)
    
    
    
}