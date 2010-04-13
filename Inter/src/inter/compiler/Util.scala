package inter.compiler

import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position

object Util {
    def javaReaderFromPath(path: String) = javaReaderFromFile(new java.io.File(path))
    def javaReaderFromFile(file: java.io.File) = new java.io.FileReader(file)
    
    def withPosOf[P <: Positional, Q <: Positional](from: P, to: Q): Q = {
        to.setPos(from.pos); to
    }
    
    def sameLength(lst1: List[_], lst2: List[_]) = (lst1.length == lst2.length)
    
    // ___ Extensions to Seq ________________________________________________
    
    class ExtendedSeq[E](seq: Seq[E]) {
        def firstSome[F](func: (E => Option[F])) = seq.foldLeft[Option[F]](None) {
            case (None, elem) => func(elem)
            case (result, _) => result
        }
    }
    implicit def extendedSeq[E](seq: Seq[E]) = new ExtendedSeq(seq)
    
    
    
}