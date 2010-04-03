package inter

object Util {
    def javaReaderFromPath(path: String) = javaReaderFromFile(new java.io.File(path))
    def javaReaderFromFile(file: java.io.File) = new java.io.FileReader(file)
    
    // ___ Extensions to Seq ________________________________________________
    
    class ExtendedSeq[E](seq: Seq[E]) {
        def firstSome[F](func: (E => Option[F])) = seq.foldLeft[Option[F]](None) {
            case (None, elem) => func(elem)
            case (result, _) => result
        }
    }
    implicit def extendedSeq[E](seq: Seq[E]) = new ExtendedSeq(seq)
}