package inter

object Util {
     def javaReaderFromPath(path: String) = javaReaderFromFile(new java.io.File(path))
     def javaReaderFromFile(file: java.io.File) = new java.io.FileReader(file)
 }