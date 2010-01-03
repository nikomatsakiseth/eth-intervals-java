package ch.ethz.intervals

import scala.collection.mutable.ListBuffer

class TranslateContext(
    val log: Log
) {
    val cds = new ListBuffer[ir.ClassDecl]()
}