package ch.ethz.intervals

import org.scalatest.SuperSuite
import java.io.File

class TestAll extends SuperSuite(List(
    new TestAnalysis(),
    new TestPlugin()
))

object TestAll {
    val DEBUG_DIR = new File("debug-logs")
}